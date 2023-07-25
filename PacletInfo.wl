(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "JerryI/WLX",
    "Description" -> "Wolfram Script XML",
    "Creator" -> "Kirill Vasin",
    "License" -> "MIT",
    "PublisherID" -> "JerryI",
    "Version" -> "0.0.1",
    "WolframVersion" -> "11+",
    "PrimaryContext" -> "JerryI`WLX`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {{"JerryI`WSP`", "WLX.wl"}, {"JerryI`WLX`Importer`", "Importer.wl"}},
        "Symbols" -> {}
      },
 
      {
        "Asset",
        "Assets" -> {
          {"ReadMe", "./README.md"},
          {"ExamplesFolder", "./TestData"},
          {"Example", "./Test.wls"}
        }
      }
    }
  |>
]
