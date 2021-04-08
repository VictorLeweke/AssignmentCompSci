// This script implements our interpreter

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

#load "InitTypesAST.fs"
open InitTypesAST
#load "InitParser.fs"
open InitParser
#load "InitLexer.fs"
open InitLexer

#load "SupportFunctions.fsx"
open SupportFunctions 

// Generate .fs files from .fsp and .fsl respectively: 
//fslexyacc\10.0.0\build\fslex\net46\fslex.exe ProjectLexer.fsl --unicode
//fslexyacc\10.0.0\build\fsyacc\net46/fsyacc.exe ProjectParser.fsp --module ProjectParser

// Init-map files
//fslexyacc\10.0.0\build\fslex\net46\fslex.exe InitLexer.fsl --unicode
//fslexyacc\10.0.0\build\fsyacc\net46/fsyacc.exe InitParser.fsp --module InitParser


// clean up code!
// finish deterministic version.
// new questions?



// Used to make strings for the nodes when creating the graphviz.
// e is an abbreviation for 'expression', i.e. a GCL expression.  
// flag is the flag for either deterministic or non-deterministic GCL.
let rec prettyPrinterList (e : cmd) (flag : char ) =
    let rec prettyListCmd c flag = 
        match c with
        | Assignment(x,y) -> [x + " = " + prettyListAri(y)]
        | If(x) -> if (flag.Equals('N')) then (prettyListGCN(x) flag)  
                                         else tupleHead (prettyListGCD(x) flag False)
        | Then(x,y) -> prettyListCmd x flag @ prettyListCmd y flag
        | Do(x) -> if (flag.Equals('N')) then (prettyListDoneN x flag @  prettyListGCN x flag)  
                                         else (tupleHead (prettyListDoneD x flag False) @ (tupleHead (prettyListGCD x flag False)))
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
    and prettyListGCN gc flag =
        match (gc) with
        | Arrow(x,y) -> ([prettyListBoolean(x) ] @ prettyListCmd y flag)
        | Ifelse(x,y) -> prettyListGCN x flag @ prettyListGCN y flag
    and prettyListGCD gc flag tests : (string list * boolean) =
        match (gc) with
        | Arrow(x,y) -> ([prettyListBoolean(Ands(x,Not(tests))) ] @ prettyListCmd y flag),(Ors(x,tests))
        | Ifelse(x,y) -> let first =  prettyListGCD x flag tests
                         let second = (prettyListGCD y flag (tupleTail first))
                         (tupleHead first @ tupleHead second,tupleTail second)
    and prettyListDoneN gc flag : string list =
        match gc with   
        | Arrow(x,y) -> [ "(" + prettyListBoolean(Not(x)) + ")"]
        | Ifelse(x,y) -> let res = (fun a b -> a + "&&" + b) (List.head (prettyListDoneN x flag)) (List.head (prettyListDoneN y flag))
                         [res]
    and prettyListDoneD gc flag tests =
        match gc with   
        | Arrow(x,y) -> ([ "(" + prettyListBoolean(Not(Ors(x,tests))) + ")" ],Ors(x,tests))
        | Ifelse(x,y) -> let first = prettyListDoneD x flag tests
                         let second = prettyListDoneD y flag (tupleTail first)
                         (tupleHead second,tupleTail second)
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
    
    prettyListCmd e flag;;


// C for counter, to create a 'fresh' node
let mutable C = 0;;
let incrementC = fun n -> (C <- C + 1);;

// Function for creating the sets of edges. edge type is a tuple of int*ANode*int, with the two integers
// representing the connected nodes, and ANode is an action (such as condition/boolean, array assignment, skip, etc)
let edges cmd (start : int ) (fin : int) (flag : char) : (int * ANode *int) list = 
    let rec edgesCMD cmd start fin flag : (int * ANode *int) list  =
        match (cmd) with
        | Assignment(x,y) -> [(start,ActAssign(cmd),fin)]
        | If(x) -> if (flag.Equals('N')) then edgesGCN x start fin flag 
                                         else tupleHead (edgesGCD x start fin flag False)
        | Do(x) -> if (flag.Equals('N')) then boolDoneN x start fin @ edgesGCN x start (start) flag
                                         else tupleHead (boolDoneD x start fin False) @ tupleHead (edgesGCD x start start flag False)
        | Then(x,y) -> incrementC();
                       let d = (C)
                       edgesCMD x start d flag @ edgesCMD y d fin flag
        | ArrayAs(x,y,z) -> [(start,ActArrAssign(cmd),fin)]
        | Null -> [(start,ActSkip(cmd),fin)];
    and edgesBool (boolean : boolean) start fin : (int * ANode *int) list =
        match (boolean) with
        | _ -> [(start,ActBool(boolean),fin)]
    and edgesGCN gc start fin flag : (int * ANode *int) list = 
        match (gc) with
        | Arrow(x,y) -> incrementC();
                            edgesBool x start C @ edgesCMD y C fin flag
        | Ifelse(x,y) -> edgesGCN x (start) (fin) flag @ edgesGCN y (start) fin flag 
    and edgesGCD gc start fin flag (tests : boolean) : (int * ANode *int) list * boolean = 
        match (gc) with
        | Arrow(x,y) -> incrementC();
                            (edgesBool (Ands(x,Not(tests))) start C @ edgesCMD y C fin flag,Ors(x,tests))
        | Ifelse(x,y) -> let first = edgesGCD x start fin flag tests
                         let second = edgesGCD y start fin flag (tupleTail first)
                         (tupleHead first @ tupleHead second,tupleTail second)  
    and boolDoneN gc start fin : (int * ANode * int) list =
        match (gc) with
        | Arrow(x,y) ->
                         edgesBool (Not(x)) start fin
        | Ifelse(x,y) -> 
        let res = (fun (a,ActBool(b),c) (d,ActBool(e),f) -> (a,ActBool(Andd(b,e)),c) ) (List.head (boolDoneN x start fin)) (List.head (boolDoneN y start fin));
        [res]
    and boolDoneD gc start fin (tests : boolean) : (int * ANode * int) list * boolean =
        match (gc) with
        | Arrow(x,y) -> ((edgesBool (Not(Ors(x,tests))) start fin),Ors(x,tests))
        | Ifelse(x,y) -> let first = boolDoneD x start fin tests
                         let second = boolDoneD y start fin (tupleTail first)
                         (tupleHead second,tupleTail second)
    edgesCMD cmd start fin flag;;



//Functions for evaluating the cmd from a memory, returning the resulting memory.
//memory is a type tuple of two dictionaries, which stores the assigned variables, and assigned arrays.
//we use the name 'maps' as variable name for the memory
let rec evalCMD cmd (maps: memory) =
    match (cmd,maps) with
    | Assignment(v,value),(vars,arrs) -> (vars.Add(v,(evalAri value maps)),arrs) 
    | ArrayAs(v,ind,value),(vars,arrs) -> (vars, arrs.Add(v,
                                            (replElemList (arrs.[v]) (evalAri ind maps) (evalAri value maps))))
    | _ -> maps // Should probably return an error
and evalAri ari (maps : memory) : int = 
    match (ari,maps) with
    | Num(x),_ -> x
    | TimesExpr(x,y),_ -> (evalAri x maps) * (evalAri y maps)
    | DivExpr(x,y),_ -> (evalAri x maps) / (evalAri y maps)
    | PlusExpr(x,y),_ -> (evalAri x maps) + (evalAri y maps)
    | MinusExpr(x,y),_ -> (evalAri x maps) - (evalAri y maps)
    | PowExpr(x,y),_ -> let yCalc = (evalAri y maps) 
                        (evalAri x maps) * yCalc * yCalc 
    | UPlusExpr(x),_ ->  +(evalAri x maps)
    | UMinusExpr(x),_ -> -(evalAri x maps)
    | Array(v,ind),(_,arrs) -> arrs.[v].[int (evalAri ind maps)]
    | Var(v),(vars,_) -> vars.[v]
and evalBool boo (maps : memory) : bool =
    match (boo) with
    | True -> true
    | False -> false
    | Ands(x,y) | Andd(x,y) -> (evalBool x maps) && (evalBool y maps)
    | Ors(x,y) | Ord(x,y) -> (evalBool x maps) || (evalBool y maps)
    | Not(x) -> not(evalBool x maps)
    | Equal(x,y) -> (evalAri x maps) = (evalAri y maps)
    | NotEq(x,y) -> (evalAri x maps) <> (evalAri y maps)
    | Greater(x,y) -> (evalAri x maps) > (evalAri y maps)
    | GreaterEq(x,y) -> (evalAri x maps) >= (evalAri y maps)
    | Less(x,y) -> (evalAri x maps) < (evalAri y maps)
    | LessEq(x,y) -> (evalAri x maps) <= (evalAri y maps)

//'Adaptor' for going from ANode to our AST types 
let eval (action : ANode) (maps : memory ) : (bool* memory) =
    match (action) with
    | ActSkip(_)-> (true,maps) 
    | ActAssign(x) -> (true,(evalCMD x maps))
    | ActArrAssign(x) -> (true,(evalCMD x maps))
    | ActBool(x) -> (evalBool x maps,maps);;

// Interprets the deterministic GCL, wrapping it into nicely formatted string result.
let rec interp (gcl : edge list) (moves : int) (currentNode : int ) (maps : memory) (bkupgcl : edge list ) (counter : int) : string =
    match(gcl,moves) with
    | (_,0) -> "Node: " + string (currentNode) + "\nCompleted in: " + string(counter) + " steps\n" + (neatMapString (tupleHead maps )) + (neatMapString (tupleTail maps ))
    | [],_ -> "Node: " + string (currentNode) + "\nCompleted in: " + string(counter) + " steps\n" + (neatMapString (tupleHead maps )) + (neatMapString (tupleTail maps ))
    | ((start,act,fin)::tail),_ -> if currentNode = start && (tupleHead (eval act maps)) then (interp bkupgcl (moves-1) fin (tupleTail (eval act maps)) bkupgcl (counter+1)) else (interp tail moves currentNode maps bkupgcl counter);;

// Same as for above, except non-deterministic.
let rec interpN (gcl : edge list) (moves : int) (currentNode : int ) (maps : memory) (bkupgcl : edge list ) (counter : int) : string =
    match(gcl,moves) with
    | (_,0) -> "Node: " + string (currentNode) + "\nCompleted in: " + string(counter) + " steps\n" + (neatMapString (tupleHead maps )) + (neatMapString (tupleTail maps ))
    | [],_ -> "Node: " + string (currentNode) + "\nCompleted in: " + string(counter) + " steps\n" + (neatMapString (tupleHead maps )) + (neatMapString (tupleTail maps ))
    | ((start,act,fin)::tail),_ -> let shuffled = shuffle bkupgcl
                                   if currentNode = start && (tupleHead (eval act maps)) then (interp shuffled (moves-1) fin (tupleTail (eval act maps)) shuffled (counter+1)) else (interp tail moves currentNode maps shuffled counter);;


// Creates the strings matching a graphviz format for the GCL code.
let graphViz cmd startVal finVal (flag : char) =
    let rec graphHelp (e : (int * ANode * int ) List) (strL : string list): string =
        match (e,strL) with
        | (_,[]) -> ""
        | ([],_) -> ""
        | ((start,n,fin)::tail,strH::strT) -> sprintf "%s -> %s [ label = \"%s \" ];\n" (string start) (string fin) (string strH) + graphHelp tail strT
    "digraph program_graph {rankdir=LR;\nnode [shape = doublecircle]; 99;\nnode [shape = circle]\n" + graphHelp (edges cmd startVal finVal flag) (prettyPrinterList cmd flag) + "}"


// A function for generating matching types of AST by parsing input
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = ProjectParser.start ProjectLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res;;

//Function for parsing the initial-conditions input.
let initParse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = InitParser.start InitLexer.tokenize lexbuf
    res;;

// Evaluates the parsed initial conditions to create a memory 
// vM = variable Memory, aM = array Memory
let initEval input : memory =
    let rec initVars var (maps : memory) = 
        match (var,maps) with
        | Comma(x,y),(_,_) -> initVars y (initVars x maps)
        | IArray(x,y),(_,_) -> initLst x y maps []
        | IAssign(x,y),(vM,aM) -> (vM.Add (x,(initNums (y))),aM)
    and initLst str values (maps : memory) arr : memory =
        match (values,maps) with
        | LstComma(x,y),(_,_) -> initLst str y maps (arr @ ([initNums x]))  
        | LstNum(x),(vM,aM) -> (vM,aM.Add (str,arr))
    and initNums nums : int =
        match (nums) with
        | UMinus(x) -> -(x)
        | INum(x) -> x
    initVars input (Map.empty,Map.empty);;


// We implement here the function that interacts with the user
let rec compute n (flag : char) (startVal : int) (finVal : int) (moves : int) =
    C <- 0; //Reset the counter every time we create a new set of edges.
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter a GCL expression: "
        try
        let e = parse (Console.ReadLine()) 
        let edgeList = (edges e startVal finVal flag)
        printf "Enter initial conditions: "
        let initMaps = initEval( initParse (Console.ReadLine()) )
        printfn "%A" (tupleHead initMaps)
        if flag = 'D' then printfn "res: %A" (interp edgeList moves startVal (initMaps) edgeList startVal) 
        else if flag = 'N' then printfn "res: %A" (interpN (shuffle edgeList) moves startVal (initMaps) (shuffle edgeList) startVal)
        compute n flag startVal finVal moves
        with err -> printfn "Error in your code" 
        compute (n-1) flag startVal finVal moves;;

// A wrapper for the functions, takes 1 argument of 'D' or 'N', the flag for Deterministic or Non-deterministic
let wrapper flag =
    let startVal = 0
    let finVal = 99
    let moves = 100
    compute 1 flag startVal finVal moves
    
// Start interacting with the user, 1 attempt!
wrapper 'N'

