#!/usr/bin/env wolframscript
(* ::Package:: *)
Get["https://raw.githubusercontent.com/JerryI/wl-localpackages/main/Kernel/LPM.wl"]

SetDirectory[If[StringQ[NotebookDirectory[] ], NotebookDirectory[], DirectoryName[$InputFileName] ] ]

PacletRepositories[{
  Github -> "https://github.com/KirillBelovTest/Objects",
  Github -> "https://github.com/KirillBelovTest/Internal",
  Github -> "https://github.com/JerryI/CSocketListener" -> "dev2024",
  Github -> "https://github.com/KirillBelovTest/TCPServer",
  Github -> "https://github.com/KirillBelovTest/HTTPHandler"
}, "Passive"->True]

<<KirillBelov`CSockets`
<<KirillBelov`Objects`
<<KirillBelov`Internal`
<<KirillBelov`TCPServer`

<<KirillBelov`HTTPHandler`
<<KirillBelov`HTTPHandler`Extensions`

Get[FileNameJoin[{ParentDirectory[Directory[]]//ParentDirectory, "Kernel", "WLX.wl"}]]
Get[FileNameJoin[{ParentDirectory[Directory[]]//ParentDirectory, "Kernel", "Importer.wl"}]]

Print["Staring HTTP server..."];

tcp = TCPServer[];
tcp["CompleteHandler", "HTTP"] = HTTPPacketQ -> HTTPPacketLength;
tcp["MessageHandler", "HTTP"] = HTTPPacketQ -> http;

index := ImportComponent["Index.wlx"];


http = HTTPHandler[];

http["MessageHandler", "Index"] = AssocMatchQ[<|"Method" -> "GET"|>] -> Function[x,  index[x] ]

(* ::End::*)
SocketListen[CSocketOpen["127.0.0.1:8010"], tcp@# &];


"open http://127.0.0.1:8010/"//Print;
While[True, Pause[1];];

