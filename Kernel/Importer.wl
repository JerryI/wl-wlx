BeginPackage["JerryI`WLX`Importer`", {"JerryI`WLX`"}]

ImportComponent::usage = "s = ImportComponent[filename_String] imports a component into a given symbol"

Placebo::usage = "Identity, but for strings"

TemplateSwizzle::usage = "Use to override components"

Begin["`Private`"]

TemplateSwizzle = <||>;

PackageExists = True

Placebo[str__] := StringJoin @@ (ToString /@ List[str])
Placebo[str_] := ToString[str]

(* evaluation tricks *)
EvaluationHolderObject /: SetDelayed[symbol_, EvaluationHolderObject[obj_, assoc_]] := With[{p = assoc["Path"]}, (
    symbol[rules_Rule] := Block[{Global`$WLXInputPath = DirectoryName[p], Global`$Children = {}, Global`$FirstChild = Null, Global`$Options = Association[rules // List]}, ReleaseHold[obj]];
    symbol[rules__Rule] := Block[{Global`$WLXInputPath = DirectoryName[p], Global`$Children = {}, Global`$FirstChild = Null, Global`$Options = Association[rules // List]}, ReleaseHold[obj]];
    symbol[arg_] := Block[{Global`$WLXInputPath = DirectoryName[p], Global`$Children = {arg}, Global`$FirstChild = arg, Global`$Options = Association[{}]}, ReleaseHold[obj]];
    symbol[arg_, rules__Rule] := Block[{Global`$WLXInputPath = DirectoryName[p], Global`$Children = {arg}, Global`$FirstChild = arg, Global`$Options = Association[rules // List]}, ReleaseHold[obj]];
    symbol[arg_, rest___, rules___Rule] := Block[{Global`$WLXInputPath = DirectoryName[p], Global`$Children = List[arg, rest], Global`$FirstChild = arg, Global`$Options = Association[rules // List]}, ReleaseHold[obj]];
)]
SetAttributes[EvaluationHolderObject, HoldFirst]

importComponent[filename_, opts___] := (
    (* check the cache first! *)
    cache[loadData[filename, opts], cinterval]
)

ImportComponent[filename_, opts___] := (
    If[StringTake[filename, -3] === ".wl", loadNormalWlFile[filename], 
        (* check the cache first! *)
        With[{object = cache[loadData[filename, opts], cinterval]},
            With[{p = object[[2]]["Path"], body = object[[1]]}, 
                Block[{Global`$WLXInputPath = DirectoryName[p]}, 
                    ReleaseHold[body] 
                ] 
            ]
        ]
    ]
)

ImportComponent /: SetDelayed[symbol_, ImportComponent[args_, opts___]] := With[{e = importComponent[args, opts]}, SetDelayed[symbol, e]]

loadNormalWlFile[filename_String] := Module[{data, path},
    (* search by relative first *)
    path = {Global`$WLXInputPath, filename // processFileName} // Flatten // FileNameJoin;
    If[!TrueQ[FileExistsQ[path]],
        path = filename // processFileName // FileNameJoin;
    ];
    
    Get[path]
]

loadData[filename_String, opts_: Rule["Localize", True]] := Module[{data, path},
    (* search by relative first *)
    path = {Global`$WLXInputPath, filename // processFileName} // Flatten // FileNameJoin;
    If[!TrueQ[FileExistsQ[path ] ],
        path = filename // processFileName // FileNameJoin;
    ];

    (* check swizzle *)

    If[KeyExistsQ[TemplateSwizzle, Hash[path] ],
        Echo["Swizzle!!!!"];
        Echo[path];
        data = Import[TemplateSwizzle[path // Hash], "Text"];
    ,
        data = Import[path, "Text"];
    ];
    

    With[{body = ProcessString[data, opts]},
        EvaluationHolderObject[body, <|"Path"->(path // FileNameJoin)|>]
    ]
]

loadData[path_, opts_: Rule["Localize", True]] := Module[{data},
    data = Import[path, "Text"];

    With[{body = ProcessString[data, opts]},
        EvaluationHolderObject[body, <|"Path"->path|>]
    ]
]


(* filenames corrector *)
checkIfUNIXStyle[str_String] := Length[StringCases[str, "\\"]] === 0

convertToUNIX[str_String] := StringReplace[str, "\\" -> "/"] // FileNameSplit
convertToWin[str_String]  := StringReplace[str, "/" -> "\\"] // FileNameSplit

processFileName[filename_String] := 
    Switch[
        $OperatingSystem,
        "MacOSX",
            If[checkIfUNIXStyle[filename], filename // FileNameSplit, convertToUNIX[filename]]
        ,
        "Windows",
            If[checkIfUNIXStyle[filename], convertToWin[filename], filename // FileNameSplit]
        ,
        _,
            If[checkIfUNIXStyle[filename], filename // FileNameSplit, convertToUNIX[filename]]
    ];


(* smart caching. credits https://github.com/KirillBelovTest *)

cinterval = "Minute"

wcache[expr_, date_DateObject] := (
	wcache[expr, {"Date"}] = date; 
	wcache[expr, date] = expr
);

wcache[expr_, interval_String: "Minute"] := (
	If[DateObjectQ[wcache[expr, {"Date"}]] && DateObject[Now, interval] != wcache[expr, {"Date"}], 
		wcache[expr, wcache[expr, {"Date"}]] =.]; 
	wcache[expr, DateObject[Now, interval]]
);

SetAttributes[wcache, HoldFirst]

cache = Identity[#1]&

Print["Caching is not enabled! Type JerryI`WLX`Importer`CacheControl[True] to set it for all IO operations"];

cache = wcache; cinterval = "Hour";

CacheContol[False] := (cache = Identity; "Caching was disabled") 
CacheContol[True]  := (cache = wcache; cinterval = "Minute"; "Caching was enabled for 1 minute") 
CacheContol["Minute"]  := (cache = wcache; cinterval = "Minute"; "Caching was enabled for 1 minute") 
CacheContol["Hour"]  := (cache = wcache; cinterval = "Hour"; "Caching was enabled for 1 hour") 

End[]

EndPackage[]