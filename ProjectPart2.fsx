// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "fslexyacc.runtime/10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "ProjectTypesAST.fs"
open ProjectTypesAST
#load "ProjectParser.fs"
open ProjectParser
#load "ProjectLexer.fs"
open ProjectLexer


// Generate .fs files from .fsp and .fsl respectively: 
//fslexyacc\10.0.0\build\fslex\net46\fslex.exe ProjectLexer.fsl --unicode
//fslexyacc\10.0.0\build\fsyacc\net46/fsyacc.exe ProjectParser.fsp --module ProjectParser

// Test a bit more
// clean up code
// upload to github 
// new questions?


let rec prettyPrinterList (e : cmd) (flag : char ) =
    let rec prettyListCmd c flag = 
        match c with
        | Assignment(x,y) -> [x + " = " + prettyListAri(y)]
        | If(x) ->  prettyListGC(x) flag
        | Then(x,y) -> prettyListCmd x flag @ prettyListCmd y flag
        | Do(x) -> prettyListDone x flag @  prettyListGC x flag
        | ArrayAs(x,y,z) -> [x + "[" + prettyListAri(y) + "] = " + prettyListAri(z)]
        | Null -> ["skip"]
    and prettyListAri a =
        match a with
        | Num(x) -> string x
        | TimesExpr(x,y) -> prettyListAri(x) + " * " +  prettyListAri (y)
        | DivExpr(x,y) -> prettyListAri(x) + " / " + prettyListAri (y)
        | PlusExpr(x,y) -> prettyListAri(x) + " + " + prettyListAri (y)
        | MinusExpr(x,y) -> prettyListAri(x) + " - " + prettyListAri(y)
        | PowExpr(x,y) -> prettyListAri(x) + "^" + "(" + prettyListAri(y) + ") "
        | UPlusExpr(x) -> prettyListAri(x)
        | UMinusExpr(x) -> "-" + prettyListAri(x)
        | Array(x,y) -> x + "[" + prettyListAri(y) + "] "
        | Var(x) -> x
    and prettyListGC gc flag =
        match (gc,flag) with
        | Arrow(x,y),'N' -> ([prettyListBoolean(x) ] @ prettyListCmd y flag)
        | Arrow(x,y),'D' -> ([prettyListBoolean(Ands(x,Not(False))) ] @ prettyListCmd y flag)
        | Ifelse(x,y),'N' -> prettyListGC x flag @ prettyListGC y flag
        | Ifelse(x,y),'D' -> let res = (fun a b -> b + "& + !(" + a + ")" ) (List.head (prettyListGC x flag)) (List.head (prettyListGC y flag))
                             prettyListGC x flag @ [res] @ (List.tail (prettyListGC y flag))
    and prettyListBoolean b = 
        match b with
        | True -> "True "
        | False -> "False "
        | Ands(x,y) -> prettyListBoolean(x) + " & " + prettyListBoolean(y)
        | Ors(x,y) -> prettyListBoolean(x) + " | " + prettyListBoolean(y)
        | Andd(x,y) -> prettyListBoolean(x) + " && " + prettyListBoolean(y)
        | Ord(x,y) -> prettyListBoolean(x) + " || " + prettyListBoolean(y)
        | Not(x) -> " not(" + prettyListBoolean(x) + ")"
        | Equal(x,y) -> "(" + prettyListAri(x) + " == " + prettyListAri(y) + ")"
        | NotEq(x,y) -> "(" +  prettyListAri(x) + " != " + prettyListAri(y) + ")"
        | Greater(x,y) -> "(" + prettyListAri(x) + " > " + prettyListAri(y) + ")"
        | GreaterEq(x,y) -> "(" + prettyListAri(x) + " >= " + prettyListAri(y) + ")"
        | Less(x,y) -> "(" +  prettyListAri(x) + " < " + prettyListAri(y) + ")"
        | LessEq(x,y) -> "(" +  prettyListAri(x) + " <= " + prettyListAri(y) + ")"
    and prettyListDone gc flag =
        match gc with   //Add flag thing here..
        | Arrow(x,y) -> [ "(" + prettyListBoolean(Not(x)) + ")"] // @ prettyListCmd(y) 
        | Ifelse(x,y) -> let res = (fun a b -> a + "&&" + b) (List.head (prettyListDone x flag)) (List.head (prettyListDone y flag))
                         [res]
    prettyListCmd e flag;;



// C for counter
let mutable C = 0;;
let incrementC = fun n -> (C <- C + 1);;

let edges cmd (start : int ) (fin : int) : (int * Node *int) list = 
    let rec edgesCMD cmd start fin : (int * Node *int) list  =
        match (cmd) with
        | Assignment(x,y) -> [(start,Cmd(cmd),fin)]
        | If(x) -> edgesGC x start fin
        | Do(x) -> boolDone x start fin @ edgesGC x start (start) 
        | Then(x,y) -> incrementC();
                       let d = (C)
                       edgesCMD x start d @ edgesCMD y d fin
        | ArrayAs(x,y,z) -> [(start,Cmd(cmd),fin)]
        | Null -> [(start,Cmd(cmd),fin)];
    and edgesBool (boolean : boolean) start fin : (int * Node *int) list =
        match (boolean) with
        | True -> [(start, Boolean(boolean),fin)]
        | False -> [(start,Boolean(boolean),fin)]
        | Ands(x,y) -> [(start,Boolean(boolean),fin)]
        | Ors(x,y) -> [(start,Boolean(boolean),fin)]
        | Andd(x,y) -> [(start,Boolean(boolean),fin)]
        | Ord(x,y) -> [(start,Boolean(boolean),fin)]
        | Not(x) -> [(start,Boolean(boolean),fin)]
        | Equal(x,y) -> [(start,Boolean(boolean),fin)]
        | NotEq(x,y) -> [(start,Boolean(boolean),fin)]
        | Greater(x,y) -> [(start,Boolean(boolean),fin)]
        | GreaterEq(x,y) -> [(start,Boolean(boolean),fin)]
        | Less(x,y) -> [(start,Boolean(boolean),fin)]
        | LessEq(x,y) -> [(start,Boolean(boolean),fin)]
    and edgesGC gc start fin : (int * Node *int) list = 
        match (gc) with
        | Arrow(x,y) -> incrementC();
                            edgesBool x start C @ edgesCMD y C fin  
        | Ifelse(x,y) -> edgesGC x (start) (fin) @ edgesGC y (start) fin

    and boolDone gc start fin : (int * Node * int) list =
        match (gc) with
        | Arrow(x,y) -> //incrementC();
                         edgesBool (Not(x)) start fin //@ edgesCMD y C fin
        | Ifelse(x,y) -> 
        let res = (fun (a,Boolean(b),c) (d,Boolean(e),f) -> (a,Boolean(Andd(b,e)),c) ) (List.head (boolDone x start fin)) (List.head (boolDone y start fin));
        [res]
    edgesCMD cmd start fin;;
// Should we use a type like "Node"
// Should our "Done" function work this way?
// Should our output be a single-element list?
// Best/easiest way to ensure fresh nodes? List of used nodes? (how to update the rest of the calls in the stack/heap)
// Implementation of deterministic version : how to?

let graphViz CMD (flag : char) =
    let rec graphHelp (e : (int * Node * int ) List) (strL : string list): string =
        match (e,strL) with
        | (_,[]) -> ""
        | ([],_) -> ""
        | ((start,n,fin)::tail,strH::strT) -> sprintf "%s -> %s [ label = \"%s \" ];\n" (string start) (string fin) (string strH) + graphHelp tail strT
    "digraph program_graph {rankdir=LR;\nnode [shape = doublecircle]; 99;\nnode [shape = circle]\n" + graphHelp (edges CMD 0 99) (prettyPrinterList CMD flag) + "}"



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
    C <- 0;
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter a GCL expression: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it 
        //printfn "Result: %A" (prettyPrinterList e)
        // printfn "Result: %A" ((edges e 0 99))
        printfn "Result: %A" (graphViz e 'N') //Flag - D = deterministic, N = Non-deterministic.  
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
compute 1

//Non-deterministic tests:
//test 1: Succesful after removing '@ prettyListCmd(y)' from prettyListDone.
//test 2: Succesful - also has the same numbers
//test 3: completely succesful
//test 4: we changed 'break' to 'skip' - successful after correctly implementing 'skip' command.
//test 5: cant test - no try/catch implementation.
//test 6: Succesful, added parenthesis to some boolean string outputs.