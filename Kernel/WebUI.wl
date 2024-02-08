BeginPackage["JerryI`WLX`WebUI`", {"JerryI`WLX`Importer`", "JerryI`WLX`", "KirillBelov`WebSocketHandler`", "JerryI`Misc`Events`", "JerryI`Misc`Events`Promise`"}]

WebUILazyLoad::usage = ""
WebUISubmit::usage = ""
WebUILocation::usage = ""

WebUILazyLoadDataProvided::usage = ""

WebUIClose::usage = ""
WebUIRefresh::usage = ""
WebUIContainer::usage = ""
WebUIContainerChild::usage = ""

WebUIJSBind::usage = ""
WebUIOnLoad::usage = ""

WebUIAliveQ::usage = ""

WebUIEventListener::usage = ""
WebUIKeyListener::usage = ""

WebUIFetch::usage = ""

WebUIInitializationScript::usage = ""

Begin["`Private`"]
ImportComponent[FileNameJoin[{$InputFileName // DirectoryName, "WebUI.wlx"}], "Localize"->False];
End["`Private`"]

EndPackage[];