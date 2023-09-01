(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "JerryI/WLX",
    "Description" -> "Wolfram Language XML syntax extension (a superset of WL and XML)",
    "Creator" -> "Kirill Vasin",
    "License" -> "GPL-3.0-only",
    "PublisherID" -> "JerryI",
    "Version" -> "1.0.2",
    "WolframVersion" -> "11+",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          {"JerryI`WLX`", "WLX.wl"},
          {"JerryI`WLX`Importer`", "Importer.wl"},
          {"JerryI`WLX`WLJS`", "WLJS.wl"}
        },
        "Symbols" -> {}
      },
      {
        "Asset",
        "Assets" -> {
          {"ReadMe", "./README.md"},
          {"ExamplesFolder", "./Examples"},
          {"Image", "./logo.png"}
        }
      }
    },
    "PrimaryContext" -> "JerryI`WLX`"
  |>
]
