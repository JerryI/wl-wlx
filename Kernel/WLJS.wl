BeginPackage["JerryI`WLX`WLJS`"]

WLJSHeader::usage = "place scripts of WLJS libs"
WLJS::usage = "WLJS[expr] embeds any wolfram expression and executes it using WLJS"

FrontEndOnly::usage = "Hold an expression for frontend"
Offload::usage = ""

Begin["`Private`"]

SetAttributes[Offload, HoldFirst]
SetAttributes[FrontEndOnly, HoldFirst]


WLJSHeader[list_String] := With[{},
    (StringRiffle[StringTemplate["<script type=\"module\" src=\"``\"></script>"]/@StringTrim/@StringSplit[list, "\n"], "\n"])<>"<script type=\"module\">core.Offload = core.Hold;</script>"
]

WLJS[expr__] := With[{uid = CreateUUID[], class = If[StringQ[Global`Class], Global`Class, ""]},
    If[TrueQ[Global`NoVirtual],
        With[{body = CompoundExpression[expr]},
            StringTemplate["<div class=\"wljs-object ``\" id=\"``\"></div><script type=\"module\">const global = {}; await interpretate(``, {local:{}, global: global})</script>"][class, uid, ExportString[body, "ExpressionJSON", "Compact"->0]]
        ]
    ,
        With[{body = Global`FrontEndVirtual[{
            Global`AttachDOM[uid],
            CompoundExpression[expr]
        }]},
            StringTemplate["<div class=\"wljs-object ``\" id=\"``\"></div><script type=\"module\">const global = {}; await interpretate(``, {local:{}, global: global})</script>"][class, uid, ExportString[body, "ExpressionJSON", "Compact"->0]]
        ]
    ]

]

End[]

EndPackage[]