# Wolfram Language XML

![](logo.png)

*a syntax extension for Wolfram Language that lets you write HTML-like markup inside a Wolfram Language Script like JSX.*

__[Docs](https://jerryi.github.io/wl-wlx/) page__ (created using WLX)

WLX produces regular Wolfram Expressions, that can be used to make components, building blocks for GUI, and advanced data representation using modern tools like HTML/CSS and Javascript

```jsx
Heading[Text_] := <h2 class="tracking-tight"><Text/></h2>;

<body>                                   
    <Heading>
        Hello World
    </Heading>
</body>
```

## Installation
### Step 1
Import a paclet from Wolfram Repository
```mathematica
PacletInstall["JerryI/WLX"]
```
or if you want to have the most recent version, please, use [LPM package manager](https://github.com/JerryI/wl-localpackages)

```mathematica
PacletRepositories[{
    Github -> "https://github.com/JerryI/wl-wlx"
}]
```

### Step 2
Get the necessary packages
```mathematica
<<JerryI`WLX`
<<JerryI`WLX`Importer`
```

### Step 3
Create a `.wlx` script and import it

*index.wlx*
```jsx
TList = Table[
    <li>
        <RandomWord></RandomWord>
    </li>
, {i, 1, 10}]; 

<body>                                   
    <ul>
        <TList/>
    </ul>
</body>
```

*notebook.nb/wls*
```mathematica
document = ImportComponent["index.wlx"];
Export[FileNameJoin[{Directory[], "index.html"}], document, "String"];
SystemOpen[FileNameJoin[{Directory[], "index.html"}]];
```

__Read more about the syntax @ [Docs](https://jerryi.github.io/wl-wlx/) page__ (created using WLX)

## Simple rules are the key
This is rather simple to guess, who is who

*html tags*
```xml
<div></div>
```

*wolfram expressions*
```xml
<Div></Div>
```

## OwnValues and DownValues
Thankfully HTML/XML tag syntax allows to make it clear, where the own-values or sub-values of a given are called

*regular WL*
```mathematica
TextString[Now]
```

*WLX*
```xml
<TextString>
   <Now/>
</TextString>
```

## Passing HTML attributes
In the example below, we declare a variable called `ClassName` and then use it inside WLX by wrapping it in curly braces
```jsx
ClassName = "show";
<div class="{ClassName}" style="color: black">
   Some text...
</div>
```

## Iterators, branching, components?
HTML/XML is a markup language by its nature. Therfore it is recommended not to use explicitly `Table` or `If` expressions inside XML tags, but rather utilize Wolfram Language for that

*columns.wlx*
```jsx
Cols = Table[ 
    <div class="lg:pr-4">
        <div class="text-base leading-7 text-gray-700 ">
            <Child/>
        </div>
    </div>
, {Child, Children}]; 

<div class="{Style}">
    <Cols/>
</div>
```

*main.wlx*
```jsx
Columns = ImportComponent["columns.wlx"];

<body>
    <Columns>
        <p>
            Block of a text
        </p>
        <p>
            Next one
        </p>        
    </Columns>
</body>
```

## License
GNU GPLv3






