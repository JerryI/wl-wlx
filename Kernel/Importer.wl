BeginPackage["JerryI`WLX`Importer`", {"JerryI`WLX`"}]

ImportComponent::usage = "s = ImportComponent[filename_String] imports a component into a given symbol"

Begin["`Private`"]

(* evaluation tricks *)
Set[symbol_, EvaluationHolderObject[obj_]] ^:= (symbol := ReleaseHold[obj])
SetDelayed[symbol_, EvaluationHolderObject[obj_]] ^:= (symbol[arg_, rest___] := Block[{Global`$Children = List[arg, rest], Global`$FirstChild = arg}, ReleaseHold[obj]])
SetAttributes[EvaluationHolderObject, HoldFirst]

ImportComponent[filename_String, opts___] := (
    (* check the cache first! *)
    cache[loadData[filename, opts], cinterval]
)

SetDelayed[symbol_, ImportComponent[args_, opts___]] ^:= With[{e = ImportComponent[args, opts]}, SetDelayed[symbol, e]]

loadData[filename_String, opts_: Rule["Localize", True]] := Module[{data},
    (* check the cache first! *)
    data = ReadString[filename // processFileName // FileNameJoin, EndOfFile];

    With[{body = ProcessString[data, opts]},
        EvaluationHolderObject[body]
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

CacheContol[False] := (cache = Identity; "Caching was disabled") 
CacheContol[True]  := (cache = wcache; cinterval = "Minute"; "Caching was enabled for 1 minute") 
CacheContol["Minute"]  := (cache = wcache; cinterval = "Minute"; "Caching was enabled for 1 minute") 
CacheContol["Hour"]  := (cache = wcache; cinterval = "Minute"; "Caching was enabled for 1 hour") 

End[]

EndPackage[]