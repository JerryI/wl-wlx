BeginPackage["JerryI`WLX`", {"CodeParser`"}]

ProcessString::usage = "ProcessString[str_String] processes str to a plain WL expressions"
ToStringRiffle::usage = "ToString, but turns arrays into a string as well"

Begin["`Private`"]

ProcessString[pstr_String, OptionsPattern[]] := Module[{str, open, close, singular, tokens, tree, placeholders, escapedCode, pureWLCode, map},
    (* escape the area marked by user *)
    {str, escapedCode} = escape[pstr];

    
    (* three types of HTML tags *)
    {open, close, singular} = parseTags[str];

    (* parse all tokens *)
    tokens = SortBy[
       Join[tokenizer[#, 1, str] & /@ open, tokenizer[#, 0, str] & /@ singular, 
        tokenizer[#, -1, str] & /@ close], #["pos"][[1]] &];


    fetchInnerText[tokens, str];
    tokens = Join[tokens, fetchInnerText[tokens, str]];
    tokens = SortBy[tokens, #["pos"][[1]] &];

    

    (* build AST tree *)
    tree = ast[tokens, str];

    (* generate rules of string replacement *)
    placeholders = {generatePlaceholders[tree]}//Flatten;


    (* generate sanitized list of expressions *)
    pureWLCode = ToExpression[#, InputForm, Hold] & /@ SplitExpression[
        With[{rules = Values[placeholders]},
            StringReplacePart[str, rules[[All, 1]], rules[[All, 2]]]
        ]
    ];

    (* construct WL expression from WLX *)
    map = (ToExpression[#[[2, 1]]] -> constructWL[#[[1]]]) & /@ placeholders;
    map = With[{expr = #[[2]]}, #[[1]]->Hold[expr]] & /@ map;

    (* restore dangerous functions *)
    
    map = map  //. {FakeFlatten -> Flatten, StringRiffleFake -> StringRiffle, 
                    StringJoinFake -> StringJoin, FakeBlock -> Block, FakeHold[x_] :> x,
                    ToStringFake -> ToStringRiffle
                };

    
    (* hydrate with WLX the original code again *)
    Print[pureWLCode];
    Print[escapedCode];

    pureWLCode = pureWLCode /. map /. escapedCode;
    

    (* EXIT::1 *)
    (* return the results if no localization is specified ! *)
    If[!TrueQ[OptionValue["Localize"]], 
      With[{r = pureWLCode},
        Return[ FakeHold[CompoundExpression @@ r] /. {Hold -> Identity} /. {FakeHold -> Hold}, Module]
      ];
    ];

    (* EXIT::2 *)
    (* perform localization of variables and return the result ! *)
    convertToModule[extractLocalVariables[pureWLCode], pureWLCode] /. {Hold -> Identity} /. {FakeHold -> Hold}  
]

Options[ProcessString] = {
  "Localize" -> False
}

parseTags[str_String] := {
  StringPosition[str, RegularExpression["\\<(\\w*)(\\s*(((\\w|-)*)=\\\"([^\"]+)\")*((\\w*)=\\{([^\\<\\>]+)\\})*)*\\>"]],
  StringPosition[str, RegularExpression["(\\<\\/[^\\<|\\>|\\/]*\\>)"]],
  StringPosition[str, RegularExpression["(\\<[^\\<|\\>|\\/]*\\/\\>)"]]
}

escape[str_String] := Module[{rules = {}, populate, newstr},
  
  populate = (rules = {rules, #}) &;
  newstr = StringReplace[str, RegularExpression["<Escape>((?:(?!<Escape>)[\\s\\S])*?)<\\/Escape>"] :> escapeReplacement[populate, "$1"]];
  
  {newstr, rules // Flatten}
]

escapeReplacement[handler_, pattern_] := With[{uid = Unique[]//ToString},
  handler[ToExpression["EscapedExpr"<>uid, InputForm] -> pattern]; 
  "<EscapedExpr"<>uid<>"/>"
]

(*** Tokenizer for the WLX subset ***)

tokenizer[s_, r_, str_] := 
 With[{element = 
    (* HTML tag as a full thing *)
    
    StringTake[StringDrop[str, s[[1]] - 1], s[[2]] - s[[1]] + 1]
  },

  With[{
    (* the head of a tag (for WL functions/components starts from Capital letter) *)
    head = 
     StringCases[element, 
       RegularExpression["\\<\\/?([^\\<|\\>|\\/|\\s]*)[^\\<|\\>|\\/]?[^\\<|\\>]*[^\\<|\\>|\\/]*\\/?\\>"] -> "$1" ] // First,

    (* parameters passed as title={Title}, usually constants for the components *)     
    block = 
     StringCases[element, 
      RegularExpression["(\\w*)=\\{([\\w|\\\"|\\-|\\.|\\s|\\d]*)\\}"] -> ("$1" -> "$2") ],

    (* HTML/XML properties passed as class="{Class}", usually constants for styles and etc *)  
    prop = 
     StringCases[element, 
      RegularExpression["(\\w*)=\\\"\\{(\\w*)\\}\\\""] -> ("$1" -> "$2") ],

    (* static properties like style="background-color: red;" *)
    common = 
     StringCases[element, 
      RegularExpression[
        "([\\w|\\-]*)=\\\"([\\w|\\(|\\)|\\;|\\:|\\s|\\d|\\%|#|\\.|\\-|\\,|\\?|\\/|\\@]*)\\\""] -> ("$1" -> "$2") ]
    },


   <|"pos" -> s, "type" -> r, 
    "atom" -> 
     If[StringMatchQ[head, RegularExpression["[\\d|\\.]*"]], "Number", If[UpperCaseQ[StringTake[head, 1]], "Expression", "HTML"]], 
    "head" -> head, "native" -> UpperCaseQ[StringTake[head, 1]], 
    "block" -> block, "properties" -> prop , 
    "common" -> common|>
   ]
  ]

(* fetch the rest of the data in a form of text (trimmed strings). 
   it can be <div>Hi!</div> -> Hi!
 *)

fetchInnerText = Function[{t, str}, 
   Select[
    Module[{bra = 0},
      Table[
       bra += t[[i]]["type"];
       
       If[bra == 0, Missing[],
        With[{l = t[[i]]["pos"][[2]], r = t[[i + 1]]["pos"][[1]]},
         
         <|"pos" -> {l + 1, r - 1}, "atom" -> "Text", "block" -> {}, 
          "head" -> "WText", "type" -> 0, "native" -> True, 
          "content" -> (StringTake[StringDrop[str, l], r - l - 1] // 
             StringTrim)|>
         ]
        ]
       , {i, Length[t] - 1}]] // DeleteMissing, 
    StringLength[#["content"]] > 0 &]
   ];

(*** Absctract Syntax Tree Builder   ***)
(*   goest one time thorugh the tokens *)

(* all nested groups flats to zero-level *)
(* nested groups are fobidden. do not use them for List[List[]]! *)
TokenGroup[{Token[A___], TokenGroup[B_], n___}] := TokenGroup[Flatten[{Token[A], B, n}]]

(* i :> WL *)

ast[tokens_List, original_String] := 
 Module[{level = 0, index = 1, bra, depth, head, exp = {}, last = {}, group, tail},
  
  (* bra stands for the level of hierarch. *)
  bra = 0; depth = 0;
  If[Length[tokens] == 0, Return[Null]];
  
  (* if this is a singular token, i.e. <br/> *)
  If[(head = First[tokens])["type"] == 0, 
   If[Length[tokens] > 1, 

    (* process the rest independently *)
    Return[TokenGroup[Flatten[{Token["Singular", head], ast[Drop[tokens, 1], original]}]]], 

    (* return that one *)
    Return[Token["Singular", head]]];
   ];
  
  (* if our tokens are non-singular *)
  
  bra = 1;
  Do[
  
   With[{t = tokens[[i]] (* current tail (or head, it depends) of the tag *)},
    bra += t["type"];

    (* if the level of nesting goes back to the original position *)
    If[bra == 0,
     (* check if we found the closing tag of one we started with*)
     If[t["head"] == head["head"],
      
      (* extract the entire expression undeneath *)
      exp = Take[tokens, i];
      last = Drop[tokens, i];
      tail = t;
      Break[];
      
      ]
     ,

     (* we are still in the nested structure *)
     depth++;
     ];
    
    ]
   
   , {i, 2, Length[tokens]}];
  
  group = {};
  

  exp = If[depth > 0,
    (* if it contains subexpressions *)

    Token["Nested", head, tail, 
     ast[Drop[Drop[exp, 1], -1], original]]
    ,

    (* if not *)
    Token["Normal", head, tail]
    ];
  
  (* parse the rest *)
  If[Length[last] > 0,
   TokenGroup[Flatten[{exp, ast[last, original]}]]
   ,
   exp
   ]
  
]

(*** Placeholder generator ***)
(* to prevent errors in a regular WL parsing procedure *)

generatePlaceholders[TokenGroup[group_]] := Module[{},
  generatePlaceholders[#] & /@ group
  ]

(* generates a rule for the StringReplace function in a form of 
    Token -> {new string, position} 
*)

generatePlaceholders[Token["Nested", head_, tail_, body_]] := 
 With[{uid = CreateUUID[]},
  Token["Nested", head, tail, body] -> {"WLX[\"" <> uid <> "\"]", {head["pos"] // First, tail["pos"] // Last}}
  ]

generatePlaceholders[Token["Normal", head_, tail_]] := 
 With[{uid = CreateUUID[]},
  Token["Normal", head, tail] -> {"WLX[\"" <> uid <> "\"]", {head["pos"] // First, tail["pos"] // Last}}
  ]

generatePlaceholders[Token["Singular", head_]] := 
 With[{uid = CreateUUID[]},
 (*was replaced from Normal to Singular??*)
  Token["Singular", head] -> {"WLX[\"" <> uid <> "\"]", head["pos"]}
  ]


(* helper function *)

ToStringRiffle[expr_] := ToString[expr];
ToStringRiffle[expr_List] := StringRiffle[ToString /@ expr, "\n"];


(***     Wolfram Script X Translator          ***)
(* construcs a valid WL expression from WLX AST *)

(* singular token -> singular WL *)
constructWL[Token["Singular", head_]] := Module[{},
   If[Length@head["block"] > 0,
    makeBlock[head["block"], 
     constructWL["Singular", head["atom"], head]]
    ,
    constructWL["Singular", head["atom"], head]
    ]
   ];

(* atoms *)
constructWL["Singular", "Number", token_] := ToExpression[token["head"]]

(* atoms *)
(* any WL expressions returns WL expression *)
constructWL["Singular", "Expression", token_] := ToExpression[token["head"], InputForm, FakeHold]
constructWL["Normal", "Expression", token_] := ToExpression[token["head"], InputForm, FakeHold][]
constructWL["Nested", "Expression", token_, args_] := ToExpression[token["head"], InputForm, FakeHold] @@ args

(* any HTML tags results in strings *)
constructWL["Normal", "HTML", token_] := With[{
   props = (StringJoinFake[#[[1]], "=\"", ToExpression[#[[2]], InputForm, FakeHold@*ToString], "\""] & /@ token["properties"]),
   commns = StringJoinFake[#[[1]], "=\"", #[[2]], "\""] & /@ token["common"]
 },
   StringJoinFake["<", token["head"], " ", StringRiffleFake[Join[props, commns], " "], "></", token["head"], ">"]
];

(* any HTML tags results in strings *)
constructWL["Singular", "HTML", token_] := With[{
   props = (StringJoinFake[#[[1]], "=\"", ToExpression[#[[2]], InputForm, FakeHold@*ToString], "\""] & /@ token["properties"]),
   commns = StringJoinFake[#[[1]], "=\"", #[[2]], "\""] & /@ token["common"]
  },
   StringJoinFake["<", token["head"], " ", StringRiffleFake[Join[props, commns], " "], "/>"
  ]
];

(* regular text *)
constructWL["Singular", "Text", token_] := token["content"]

(* any HTML tags results in strings *)
constructWL["Nested", "HTML", token_, args_] := With[{
   props = (StringJoinFake[#[[1]], "=\"", ToExpression[#[[2]], InputForm, FakeHold@*ToString], "\""] & /@ token["properties"]),
   commns = StringJoinFake[#[[1]], "=\"", #[[2]], "\""] & /@ token["common"]
  },
   StringJoinFake["<", token["head"], " ", StringRiffleFake[Join[props, commns], " "], ">", StringRiffleFake[{ToStringFake /@ args} // FakeFlatten, ""], "</", token["head"], ">"
  ]
];

constructWL[Token["Normal", head_, tail_]] := 
   If[Length@(head["block"]) > 0,
    makeBlock[head["block"], constructWL["Normal", head["atom"], head]]
    ,
    constructWL["Normal", head["atom"], head]
];


constructWL[Token["Nested", head_, tail_, TokenGroup[childred_]]] := 
  Module[{},
   If[Length@head["block"] > 0,
    makeBlock[head["block"], 
     constructWL["Nested", head["atom"], head, (constructWL /@ childred)]]
    ,
     constructWL["Nested", head["atom"], head, (constructWL /@ childred)]
    ]
];

constructWL[Token["Nested", head_, tail_, child_]] := 
   If[Length@head["block"] > 0,
    makeBlock[head["block"], 
     constructWL["Nested", head["atom"], head, (constructWL /@ {child})]]
    ,
     constructWL["Nested", head["atom"], head, (constructWL /@ {child})]
];

(*** Language manipulation tools ***)

convertToModule[vars_, body_] := vars /. _@{v__} :> FakeHold[Module[{v}, CompoundExpression @@ body]]
convertToModule[Hold[{}], body_]    := FakeHold[CompoundExpression @@ body]

extractLocalVariables[exprs_List] := Module[{localVariables = {}},
  (* capture all set and setdelayed in the top-level *)

  exprs /. {
   Hold[Set[a_, b_]] :> AppendTo[localVariables, Hold[a]], 
   Hold[SetDelayed[a_, b_]] :> AppendTo[localVariables, Hold[a]], 
   Hold[CompoundExpression[SetDelayed[a_, b_], args__]] :> AppendTo[localVariables, Hold[a]],
   Hold[CompoundExpression[Set[a_, b_], args__]] :> AppendTo[localVariables, Hold[a]]
  };
  
  (* extract symbol names from DownValues and OwnValues *)
  localVariables = 
  If[
    With[{s = Extract[#, {1, 0}, Unevaluated]},
     s === Symbol
    ],
      #
  ,
      Extract[#, {1, 0}, Hold]
  ] & /@ localVariables;

  (* flatten the list *)
  With[{l = DeleteDuplicates[localVariables]},
    FakeHold[l]
  ] /. {Hold[x_] :> x} /. {FakeHold -> Hold}
]


(* split expressions in the original Wolfram Code *)

SplitExpression[astr_] := 
  With[{str = StringReplace[astr, {}]}, 
   Select[
    Select[(StringTake[str, 
         Partition[Join[{1}, #, {StringLength[str]}], 2]] &@
       Flatten[{#1 - 1, #2 + 1} & @@@ 
         Sort@
          Cases[
           CodeParser`CodeConcreteParse[str, 
             CodeParser`SourceConvention -> 
              "SourceCharacterIndex"][[2]], 
           LeafNode[Token`Newline, _, a_] :> 
            Lookup[a, Source, Nothing]]]), 
     StringQ], (StringLength[#] > 0) &]]; 

(* utils for making modules/blocks *)
SetAttributes[FakeSet, HoldAll]
SetAttributes[FakeHold, HoldAll]
SetAttributes[FakeBlock, HoldAll]

makeBlock[vars_List, inner_] :=
 (With[{c = vars /. {
    Rule[x_, y_] :> 
        With[{a = ToExpression[x, InputForm, FakeHold], b = ToExpression[y, InputForm, FakeHold]},
         FakeSet[a, b]
        ]}
    },

   With[{v = Hold[c] /. {FakeHold[x_] :> x} /. {FakeSet :> Set}},
    FakeBlock[v, 
       ArgsPlaceHolder] /. {Hold[x_] -> x} /. {ArgsPlaceHolder -> 
       FakeHold[inner]}
    ]
   ])


End[]
EndPackage[]