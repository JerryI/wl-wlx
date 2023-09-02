#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory[FileNameJoin[{If[StringQ[NotebookDirectory[]], NotebookDirectory[], DirectoryName[$InputFileName]], "Docs"}]]
Get[FileNameJoin[{ParentDirectory[Directory[]],"Kernel", "WLX.wl"}]]
Get[FileNameJoin[{ParentDirectory[Directory[]],"Kernel", "Importer.wl"}]]
Get[FileNameJoin[{ParentDirectory[Directory[]],"Kernel", "Highlighter.wl"}]]
Get[FileNameJoin[{ParentDirectory[Directory[]],"Kernel", "WLJS.wl"}]]

document = ImportComponent["index.wlx"];
document // Print;


Export[FileNameJoin[{ParentDirectory[Directory[]], "index.html"}], document, "String"];
Print["Open index.html in your browser, if it did not happen automatically"];
SystemOpen[FileNameJoin[{ParentDirectory[Directory[]], "index.html"}]]


FDate := FileDate[#]& /@ FileNames["*.wlx", Directory[]];
dates = FDate;

rebuild := Module[{},
	If[FDate === dates, Return[Null, Module], dates = FDate];
	document = ImportComponent["index.wlx"];
	
	Export[FileNameJoin[{ParentDirectory[Directory[]], "index.html"}], document, "String"];
	SystemOpen[FileNameJoin[{ParentDirectory[Directory[]], "index.html"}]];
];

task = SessionSubmit[ScheduledTask[rebuild, Quantity[3, "Seconds"]]];

While[True, Pause[0.1]];
