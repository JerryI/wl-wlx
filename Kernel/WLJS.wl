BeginPackage["JerryI`WLX`WLJS`", {"JerryI`Misc`Events`"}]

WLJSHeader::usage = "place scripts of WLJS libs"
WLJS::usage = "WLJS[expr] embeds any wolfram expression and executes it using WLJS"

FrontEndOnly::usage = "Hold an expression for frontend"
Offload::usage = ""

Begin["`Private`"]

SetAttributes[Offload, HoldFirst]
SetAttributes[FrontEndOnly, HoldFirst]

System`FrontEndVirtual;
System`AttachDOM;

WLJSHeader[list1_String, OptionsPattern[]] := With[{list = OptionValue["List"]},
    StringRiffle[StringTemplate["<script type=\"module\" src=\"``\"></script>"]/@ Join[(StringTrim/@StringSplit[list1, "\n"]), list], "\n"]
]

WLJSHeader[OptionsPattern[]] := With[{},
    StringRiffle[StringTemplate["<script type=\"module\" src=\"``\"></script>"]/@ OptionValue["List"], "\n"]
]

Options[WLJSHeader] = {"List" -> {}}

EventObject /: WLJS[EventObject[assoc_], opts:OptionsPattern[] ] := WLJS[ assoc["View"], opts ] 

WLJS[expr__, OptionsPattern[]] := With[{uid = CreateUUID[], class = OptionValue["Class"]},
    If[TrueQ[OptionValue["NoVirtual"]],
        With[{body = CompoundExpression[expr]},
            StringTemplate["<div class=\"wljs-object ``\" id=\"``\"></div><script type=\"module\">const global = {}; await interpretate(``, {local:{}, global: global})</script>"][class, uid, ExportString[body, "ExpressionJSON", "Compact"->0]]
        ]
    ,
        With[{body = FrontEndVirtual[{
            AttachDOM[uid],
            CompoundExpression[expr]
        }]},
            StringTemplate["<div class=\"wljs-object ``\" id=\"``\"></div><script type=\"module\">const global = {}; await interpretate(``, {local:{}, global: global})</script>"][class, uid, ExportString[body, "ExpressionJSON", "Compact"->0]]
        ]
    ]

]

WLJS[expr_, OptionsPattern[]] := With[{uid = CreateUUID[], class = OptionValue["Class"]},
    If[TrueQ[OptionValue["NoVirtual"]],
        With[{body = expr},
            StringTemplate["<div class=\"wljs-object ``\" id=\"``\"></div><script type=\"module\">const global = {}; await interpretate(``, {local:{}, global: global})</script>"][class, uid, ExportString[body, "ExpressionJSON", "Compact"->0]]
        ]
    ,
        With[{body = FrontEndVirtual[{
            AttachDOM[uid],
            expr
        }]},
            StringTemplate["<div class=\"wljs-object ``\" id=\"``\"></div><script type=\"module\">const global = {}; await interpretate(``, {local:{}, global: global})</script>"][class, uid, ExportString[body, "ExpressionJSON", "Compact"->0]]
        ]
    ]

]

Options[WLJS] = {"Class"->"", "NoVirtual"->False}

End[]

EndPackage[]
