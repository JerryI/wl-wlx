#!/usr/bin/env wolframscript
(* ::Package:: *)
Once[If[PacletFind["JerryI/LPM"] === {}, PacletInstall["JerryI/LPM"]]]; 
<< JerryI`LPM`

PacletRepositories[{
  Github -> "https://github.com/KirillBelovTest/Objects",
  Github -> "https://github.com/KirillBelovTest/Internal",
  Github -> "https://github.com/JerryI/CSocketListener",
  Github -> "https://github.com/JerryI/TCPServer",
  Github -> "https://github.com/JerryI/HTTPHandler",
  Github -> "https://github.com/JerryI/WebSocketHandler",
  Github -> "https://github.com/JerryI/wl-misc"
}]

<<KirillBelov`Objects`
<<KirillBelov`Internal`
<<KirillBelov`CSocketListener`
<<KirillBelov`TCPServer`

<<KirillBelov`HTTPHandler`
<<KirillBelov`HTTPHandler`Extensions`

SetDirectory[FileNameJoin[{If[StringQ[NotebookDirectory[]], NotebookDirectory[], DirectoryName[$InputFileName]], "Explorer"}]]
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


StringTemplate["open http://``:``/"][httplistener[[1]]["Host"], httplistener[[1]]["Port"]] // Print;
While[True, Pause[1]];

