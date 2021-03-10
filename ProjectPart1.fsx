// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "ProjectTypesAST.fs"
open ProjectTypesAST
#load "ProjectParser.fs"
open ProjectParser
#load "ProjectLexer.fs"
open ProjectLexer

//FsLexYacc.10.0.0/build/fslex/net46/fslex.exe ProjectLexer.fsl --unicode
//FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe ProjectParser.fsp --module ProjectParser



// We create a string list list, with the purpose of each sublist being its own "level" of a visual tree graph
let rec treeWalker (e : cmd) : string list list =
    let rec treeWalkerCMD e : string list list  =
        match e with
        | Assignment(x,y) ->  [[":="]] @ ([[x]] @ treeWalkerAri y) 
        | If(x) -> [["if gc fi "]] @ treeWalkerGC x
        | Then(x,y) -> [["then"]] @ (treeWalkerCMD x @ treeWalkerCMD y)
        | Do(x) -> [["while "]] @ treeWalkerGC x
        | ArrayAs(x,y,z) -> [[x + " [ ari ] " ]] @ (treeWalkerAri y @ treeWalkerAri z)
        | Null -> [[""]]
    and treeWalkerGC gc : string list list =
        match gc with
        | Arrow(x,y) -> [["b -> cmd"]] @ (treeWalkerBoolean x @ treeWalkerCMD y) 
        | Ifelse(x,y) -> [["else if "]] @ (treeWalkerGC x @ treeWalkerGC y)
    and treeWalkerAri a : string list list =
        match a with
        | Num(x) -> [[string x]]
        | TimesExpr(x,y) -> treeWalkerAri x @ [[" * "]] @ treeWalkerAri y
        | DivExpr(x,y) -> treeWalkerAri x @ [[" / "]] @ treeWalkerAri y
        | PlusExpr(x,y) -> treeWalkerAri x @ [[" + "]] @ treeWalkerAri y
        | MinusExpr(x,y) -> treeWalkerAri x @ [[" - "]] @ treeWalkerAri y
        | PowExpr(x,y) -> treeWalkerAri x @ [["^("]] @ treeWalkerAri y @ [[") "]]
        | UPlusExpr(x) -> [["+"]] @ treeWalkerAri x
        | UMinusExpr(x) -> [["-"]] @ treeWalkerAri x
        | Array(x,y) -> [[x]] @ [["["]] @ treeWalkerAri y @ [["] "]]
        | Var(x) -> [[x]]
    and treeWalkerBoolean b : string list list =
        match b with 
        | True -> [["True "]]
        | False -> [["False "]]
        | Ands(x,y) ->  [[" & "]] @ (treeWalkerBoolean x  @ treeWalkerBoolean y)
        | Ors(x,y) ->  [[" | "]] @ treeWalkerBoolean x  @ treeWalkerBoolean y
        | Andd(x,y) -> [[" && "]] @ treeWalkerBoolean x @ treeWalkerBoolean y
        | Ord(x,y) -> [[" || "]] @ treeWalkerBoolean x @ treeWalkerBoolean y
        | Not(x) -> [[" not( ) "]] @ treeWalkerBoolean x
        | Equal(x,y) -> [[" == "]] @ treeWalkerAri x  @ treeWalkerAri y
        | NotEq(x,y) -> [[" != "]] @ treeWalkerAri x @ treeWalkerAri y
        | Greater(x,y) -> [[" > "]] @ treeWalkerAri x  @ treeWalkerAri y
        | GreaterEq(x,y) -> [[" >= "]] @ treeWalkerAri x @  treeWalkerAri y
        | Less(x,y) -> [[" < "]] @ treeWalkerAri x @ treeWalkerAri y
        | LessEq(x,y) -> [[" <= "]] @ treeWalkerAri x @  treeWalkerAri y
    treeWalkerCMD e;;


// prettyprints GCL into more human-friendly text.
let rec prettyPrinter e  =
    let rec prettyPrinterCmd c =
        match c with
        | Assignment(x,y) -> x + " = " + prettyPrinterAri(y)
        | If(x) -> "if " + prettyPrinterGC(x) 
        | Then(x,y) -> prettyPrinterCmd(x) + ";\n" + prettyPrinterCmd(y)
        | Do(x) -> "while " + prettyPrinterGC(x)
        | ArrayAs(x,y,z) -> x + "[" + prettyPrinterAri(y) + "] = " + prettyPrinterAri(z)
        | Null -> ""
    and prettyPrinterAri a =
        match a with
        | Num(x) -> string x
        | TimesExpr(x,y) -> prettyPrinterAri(x) + " * " +  prettyPrinterAri (y)
        | DivExpr(x,y) -> prettyPrinterAri(x) + " / " + prettyPrinterAri (y)
        | PlusExpr(x,y) -> prettyPrinterAri(x) + " + " + prettyPrinterAri (y)
        | MinusExpr(x,y) -> prettyPrinterAri(x) + " - " + prettyPrinterAri(y)
        | PowExpr(x,y) -> prettyPrinterAri(x) + "^" + "(" + prettyPrinterAri(y) + ") "
        | UPlusExpr(x) -> prettyPrinterAri(x)
        | UMinusExpr(x) -> "-" + prettyPrinterAri(x)
        | Array(x,y) -> x + "[" + prettyPrinterAri(y) + "] "
        | Var(x) -> x
    and prettyPrinterGC gc =
        match gc with
        | Arrow(x,y) -> prettyPrinterBoolean(x) + " then\n" + prettyPrinterCmd(y)
        | Ifelse(x,y) -> prettyPrinterGC(x) + " else " + prettyPrinterGC(y)
    and prettyPrinterBoolean b = 
        match b with
        | True -> "True "
        | False -> "False "
        | Ands(x,y) -> prettyPrinterBoolean(x) + " & " + prettyPrinterBoolean(y)
        | Ors(x,y) -> prettyPrinterBoolean(x) + " | " + prettyPrinterBoolean(y)
        | Andd(x,y) -> prettyPrinterBoolean(x) + " && " + prettyPrinterBoolean(y)
        | Ord(x,y) -> prettyPrinterBoolean(x) + " || " + prettyPrinterBoolean(y)
        | Not(x) -> " not(" + prettyPrinterBoolean(x) + ")"
        | Equal(x,y) -> prettyPrinterAri(x) + " == " + prettyPrinterAri(y)
        | NotEq(x,y) -> prettyPrinterAri(x) + " != " + prettyPrinterAri(y)
        | Greater(x,y) -> prettyPrinterAri(x) + " > " + prettyPrinterAri(y)
        | GreaterEq(x,y) -> prettyPrinterAri(x) + " >= " + prettyPrinterAri(y)
        | Less(x,y) -> prettyPrinterAri(x) + " < " + prettyPrinterAri(y)
        | LessEq(x,y) -> prettyPrinterAri(x) + " <= " + prettyPrinterAri(y)
    prettyPrinterCmd e;;

// A function for generating matching types of AST by parsing input
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = ProjectParser.start ProjectLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    //printfn("Parsing3")
    res;;

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an arithmetic expression: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "Result: %A" (prettyPrinter e)
        compute n
        with err -> printfn "Error in your code" 
        compute (n-1);;

// Copied from https://stackoverflow.com/questions/2365527/how-read-a-file-into-a-seq-of-lines-in-f/2366649
// to be used for reading from files instead of command line.
open System.IO
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath) 
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

// Start interacting with the user, 3 attempts!
compute 3