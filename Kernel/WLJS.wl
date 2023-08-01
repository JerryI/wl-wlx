BeginPackage["JerryI`WLX`WLJS`"]

WLJSHeader::usage = "place scripts of WLJS libs"
WLJS::usage = "WLJS[expr] embeds any wolfram expression and executes it using WLJS"

FrontEndOnly::usage = "Hold an expression for frontend"

Begin["`Private`"]

SetAttributes[FrontEndOnly, HoldFirst]

WLJSHeader[list_String] := With[{},
    StringRiffle[StringTemplate["<script type=\"module\" src=\"``\"></script>"]/@StringTrim/@StringSplit[list, "\n"], "\n"]
]

WLJS[expr_] := With[{uid = CreateUUID[]},
    With[{body = Global`FrontEndVirtual[{
        Global`AttachDOM[uid],
        expr
    }]},
        StringTemplate["<div class=\"wljs-object\" id=\"``\"></div><script type=\"module\">const global = {}; await interpretate(``, {local:{}, global: global})</script>"][uid, ExportString[body, "ExpressionJSON", "Compact"->0]]
    ]
]

End[]

EndPackage[]