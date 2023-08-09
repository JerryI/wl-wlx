#!/usr/bin/env wolframscript
(* ::Package:: *)
PacletInstall["JerryI/LPM"]
<< JerryI`LPM`

PacletRepositories[{
  Github -> "https://github.com/KirillBelovTest/Objects",
  Github -> "https://github.com/KirillBelovTest/Internal",
  Github -> "https://github.com/JerryI/CSocketListener",
  Github -> "https://github.com/KirillBelovTest/TCPServer",
  Github -> "https://github.com/KirillBelovTest/HTTPHandler",
  Github -> "https://github.com/KirillBelovTest/WebSocketHandler",
  Github -> "https://github.com/JerryI/wl-misc"
}]

<<KirillBelov`Objects`
<<KirillBelov`Internal`
<<KirillBelov`CSocketListener`
<<KirillBelov`TCPServer`

<<KirillBelov`HTTPHandler`
<<KirillBelov`HTTPHandler`Extensions`

<<KirillBelov`WebSocketHandler`

<<JerryI`Misc`Events`

SetDirectory[FileNameJoin[{If[StringQ[NotebookDirectory[]], NotebookDirectory[], DirectoryName[$InputFileName]], "App"}]]
Get[FileNameJoin[{ParentDirectory[Directory[]]//ParentDirectory,"Kernel", "WLX.wl"}]]
Get[FileNameJoin[{ParentDirectory[Directory[]]//ParentDirectory,"Kernel", "Importer.wl"}]]
Get[FileNameJoin[{ParentDirectory[Directory[]]//ParentDirectory,"Kernel", "Highlighter.wl"}]]
Get[FileNameJoin[{ParentDirectory[Directory[]]//ParentDirectory,"Kernel", "WLJS.wl"}]]

Print["Staring HTTP server..."];

tcp = TCPServer[];
tcp["CompleteHandler", "HTTP"] = HTTPPacketQ -> HTTPPacketLength;
tcp["MessageHandler", "HTTP"] = HTTPPacketQ -> http;


index := ImportComponent["index.wlx"];
FileRequestQ[request_] := (!MissingQ[request["Query"]["file"]])

http = HTTPHandler[];

http["MessageHandler", "File"] = FileRequestQ -> ImportFile
http["MessageHandler", "Index"] = AssocMatchQ[<|"Method" -> "GET"|>] -> Function[x, index[x]]

(* ::End::*)
httplistener =  Check[CSocketListen["127.0.0.1:8010", tcp@# &], Print["FUCK LibraryLink and Sockets! Using shitty ZMQ..."]; SocketListen["127.0.0.1:8010", tcp@# &]];


Print["Staring WS/HTTP server..."];

wcp = TCPServer[]
wcp["CompleteHandler", "WebSocket"] = WebSocketPacketQ -> WebSocketPacketLength
wcp["MessageHandler", "WebSocket"]  = WebSocketPacketQ -> ws

ws = WebSocketHandler[]

ws["MessageHandler", "Evaluate"]  = Function[True] -> evaluate

evaluate[cl: _SocketObject | _CSocket, data_ByteArray] := Block[{Global`client = cl},
    Print[data//ByteArrayToString];
    ToExpression[data//ByteArrayToString];
]

(* polyfills from frontend *)
$DefaultSerializer = ExportByteArray[#, "ExpressionJSON"]&

NotebookPromise[uid_, params_][expr_] := With[{cli = Global`client},
    WebSocketSend[cli, Global`PromiseResolve[uid, expr] // $DefaultSerializer]
];

NotebookAddTracking[symbol_] := With[{cli = Global`client, name = SymbolName[Unevaluated[symbol]]},
    Print["Add tracking... for "<>name];
    Experimental`ValueFunction[Unevaluated[symbol]] = Function[{y,x}, WebSocketSend[cli, FrontUpdateSymbol[name, x] // $DefaultSerializer]]
]

SetAttributes[NotebookAddTracking, HoldFirst]

NotebookGetSymbol[uid_, params_][expr_] := 
    With[{result = expr // ReleaseHold},
        Print["evaluating the desired symbol on the Kernel"];
        Print["promise resolve"];
        WebSocketSend[Global`client, Global`PromiseResolve[uid, result] // $DefaultSerializer] 
    ];

NotebookPromiseKernel[uid_, params_][expr_] := With[{cli = Global`client},
    With[{result = expr // ReleaseHold},
        Print["side evaluating on the Kernel"];
        WebSocketSend[cli, Global`PromiseResolve[uid, result] // $DefaultSerializer]
    ]
];

NotebookEmitt[expr_] := ReleaseHold[expr]

Check[CSocketListen["127.0.0.1:8011", wcp@#&], Print["FUCK LibraryLink and Sockets! Using shitty ZMQ..."]; SocketListen["127.0.0.1:8011", wcp@#&]];

SetAttributes[Offload, HoldFirst];

StringTemplate["open http://``:``/"][httplistener[[1]]["Host"], httplistener[[1]]["Port"]] // Print;
While[True, Pause[1]];

