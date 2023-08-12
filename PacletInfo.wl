(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "JerryI/WLX",
    "Description" -> "Wolfram Language XML syntax extension (a superset of WL and XML)",
    "Creator" -> "Kirill Vasin",
    "License" -> "GPL3",
    "PublisherID" -> "JerryI",
    "Version" -> "1.0.0",
    "WolframVersion" -> "11+",
    "PrimaryContext" -> "JerryI`WLX`",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          {"JerryI`WSP`", "WLX.wl"}, 
          {"JerryI`WLX`Importer`", "Importer.wl"},
          {"JerryI`WLX`WLJS`", "WLJS.wl"}
        },
        "Symbols" -> {}
      },
 
      { "Asset",
        "Assets" -> {
          {"ReadMe", "./README.md"},
          {"ExamplesFolder", "./Examples"},
          {"Image", "./logo.png"}
        }
      }
    }
  |>
]
