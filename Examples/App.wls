#!/usr/bin/env wolframscript
(* ::Package:: *)
Get["https://raw.githubusercontent.com/JerryI/wl-localpackages/main/Kernel/LPM.wl"];

PacletRepositories[{
  Github -> "https://github.com/KirillBelovTest/Objects",
  Github -> "https://github.com/KirillBelovTest/Internal",
  Github -> "https://github.com/JerryI/CSocketListener" -> "dev",
  Github -> "https://github.com/KirillBelovTest/TCPServer",
  Github -> "https://github.com/KirillBelovTest/HTTPHandler",
  Github -> "https://github.com/KirillBelovTest/WebSocketHandler",
  Github -> "https://github.com/JerryI/wl-misc" -> "dev"
}]

ENV = <|
    "WSPort"->8011,
    "HTTPPort"->8010,
    "Host"->"127.0.0.1"
|>

ENV["HAddr"] := StringTemplate["``:``"][ENV["Host"], ENV["HTTPPort"]]
ENV["WAddr"] := StringTemplate["``:``"][ENV["Host"], ENV["WSPort"]]

<<KirillBelov`Objects`
<<KirillBelov`Internal`
<<KirillBelov`CSockets`
<<KirillBelov`TCPServer`

<<KirillBelov`HTTPHandler`
<<KirillBelov`HTTPHandler`Extensions`

<<KirillBelov`WebSocketHandler`

<<JerryI`Misc`Events`
<<JerryI`Misc`WLJS`Transport`

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
httplistener =  SocketListen[CSocketOpen[ENV["HAddr"]], tcp@# &]


Print["Staring WS/HTTP server..."];

wcp = TCPServer[]
wcp["CompleteHandler", "WebSocket"] = WebSocketPacketQ -> WebSocketPacketLength
wcp["MessageHandler", "WebSocket"]  = WebSocketPacketQ -> ws

ws = WebSocketHandler[]

ws["MessageHandler", "Evaluate"]  = Function[True] -> WLJSTransportHandler

WLJSTransportHandler["AddTracking"] = Function[{symbol, name, cli, callback},
    Print["Add tracking... for "<>name];
    Experimental`ValueFunction[Unevaluated[symbol]] = Function[{y,x}, callback[cli, x]];
, HoldFirst]

WLJSTransportHandler["GetSymbol"] = Function[{expr, client, callback},
    Print["evaluating the desired symbol on the Kernel"];
    callback[expr // ReleaseHold];
]

SocketListen[CSocketOpen[ENV["WAddr"]], wcp@#&];

SetAttributes[Offload, HoldFirst];

StringTemplate["open http://``"][ENV["HAddr"]] // Print;
While[True, Pause[1]];

