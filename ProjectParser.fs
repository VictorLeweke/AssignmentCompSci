// Implementation file for parser generated by fsyacc
module ProjectParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 2 "ProjectParser.fsp"

open ProjectTypesAST

# 10 "ProjectParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | TIMES
  | DIV
  | PLUS
  | MINUS
  | POW
  | LPAR
  | RPAR
  | ASGN
  | THEN
  | IF
  | FI
  | DO
  | OD
  | SKIP
  | IFELSE
  | LBRA
  | RBRA
  | TRUE
  | FALSE
  | ANDD
  | ORD
  | ANDS
  | ORS
  | NOTEQ
  | NOT
  | LTEQ
  | GTEQ
  | EQ
  | LT
  | GT
  | ARROW
  | EOF
  | VAR of (string)
  | NUM of (float)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_POW
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_ASGN
    | TOKEN_THEN
    | TOKEN_IF
    | TOKEN_FI
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_SKIP
    | TOKEN_IFELSE
    | TOKEN_LBRA
    | TOKEN_RBRA
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_ANDD
    | TOKEN_ORD
    | TOKEN_ANDS
    | TOKEN_ORS
    | TOKEN_NOTEQ
    | TOKEN_NOT
    | TOKEN_LTEQ
    | TOKEN_GTEQ
    | TOKEN_EQ
    | TOKEN_LT
    | TOKEN_GT
    | TOKEN_ARROW
    | TOKEN_EOF
    | TOKEN_VAR
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_cmd
    | NONTERM_gc
    | NONTERM_ari
    | NONTERM_boolean

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | TIMES  -> 0 
  | DIV  -> 1 
  | PLUS  -> 2 
  | MINUS  -> 3 
  | POW  -> 4 
  | LPAR  -> 5 
  | RPAR  -> 6 
  | ASGN  -> 7 
  | THEN  -> 8 
  | IF  -> 9 
  | FI  -> 10 
  | DO  -> 11 
  | OD  -> 12 
  | SKIP  -> 13 
  | IFELSE  -> 14 
  | LBRA  -> 15 
  | RBRA  -> 16 
  | TRUE  -> 17 
  | FALSE  -> 18 
  | ANDD  -> 19 
  | ORD  -> 20 
  | ANDS  -> 21 
  | ORS  -> 22 
  | NOTEQ  -> 23 
  | NOT  -> 24 
  | LTEQ  -> 25 
  | GTEQ  -> 26 
  | EQ  -> 27 
  | LT  -> 28 
  | GT  -> 29 
  | ARROW  -> 30 
  | EOF  -> 31 
  | VAR _ -> 32 
  | NUM _ -> 33 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_TIMES 
  | 1 -> TOKEN_DIV 
  | 2 -> TOKEN_PLUS 
  | 3 -> TOKEN_MINUS 
  | 4 -> TOKEN_POW 
  | 5 -> TOKEN_LPAR 
  | 6 -> TOKEN_RPAR 
  | 7 -> TOKEN_ASGN 
  | 8 -> TOKEN_THEN 
  | 9 -> TOKEN_IF 
  | 10 -> TOKEN_FI 
  | 11 -> TOKEN_DO 
  | 12 -> TOKEN_OD 
  | 13 -> TOKEN_SKIP 
  | 14 -> TOKEN_IFELSE 
  | 15 -> TOKEN_LBRA 
  | 16 -> TOKEN_RBRA 
  | 17 -> TOKEN_TRUE 
  | 18 -> TOKEN_FALSE 
  | 19 -> TOKEN_ANDD 
  | 20 -> TOKEN_ORD 
  | 21 -> TOKEN_ANDS 
  | 22 -> TOKEN_ORS 
  | 23 -> TOKEN_NOTEQ 
  | 24 -> TOKEN_NOT 
  | 25 -> TOKEN_LTEQ 
  | 26 -> TOKEN_GTEQ 
  | 27 -> TOKEN_EQ 
  | 28 -> TOKEN_LT 
  | 29 -> TOKEN_GT 
  | 30 -> TOKEN_ARROW 
  | 31 -> TOKEN_EOF 
  | 32 -> TOKEN_VAR 
  | 33 -> TOKEN_NUM 
  | 36 -> TOKEN_end_of_input
  | 34 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_cmd 
    | 3 -> NONTERM_cmd 
    | 4 -> NONTERM_cmd 
    | 5 -> NONTERM_cmd 
    | 6 -> NONTERM_cmd 
    | 7 -> NONTERM_cmd 
    | 8 -> NONTERM_gc 
    | 9 -> NONTERM_gc 
    | 10 -> NONTERM_ari 
    | 11 -> NONTERM_ari 
    | 12 -> NONTERM_ari 
    | 13 -> NONTERM_ari 
    | 14 -> NONTERM_ari 
    | 15 -> NONTERM_ari 
    | 16 -> NONTERM_ari 
    | 17 -> NONTERM_ari 
    | 18 -> NONTERM_ari 
    | 19 -> NONTERM_ari 
    | 20 -> NONTERM_ari 
    | 21 -> NONTERM_boolean 
    | 22 -> NONTERM_boolean 
    | 23 -> NONTERM_boolean 
    | 24 -> NONTERM_boolean 
    | 25 -> NONTERM_boolean 
    | 26 -> NONTERM_boolean 
    | 27 -> NONTERM_boolean 
    | 28 -> NONTERM_boolean 
    | 29 -> NONTERM_boolean 
    | 30 -> NONTERM_boolean 
    | 31 -> NONTERM_boolean 
    | 32 -> NONTERM_boolean 
    | 33 -> NONTERM_boolean 
    | 34 -> NONTERM_boolean 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 36 
let _fsyacc_tagOfErrorTerminal = 34

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | TIMES  -> "TIMES" 
  | DIV  -> "DIV" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | POW  -> "POW" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | ASGN  -> "ASGN" 
  | THEN  -> "THEN" 
  | IF  -> "IF" 
  | FI  -> "FI" 
  | DO  -> "DO" 
  | OD  -> "OD" 
  | SKIP  -> "SKIP" 
  | IFELSE  -> "IFELSE" 
  | LBRA  -> "LBRA" 
  | RBRA  -> "RBRA" 
  | TRUE  -> "TRUE" 
  | FALSE  -> "FALSE" 
  | ANDD  -> "ANDD" 
  | ORD  -> "ORD" 
  | ANDS  -> "ANDS" 
  | ORS  -> "ORS" 
  | NOTEQ  -> "NOTEQ" 
  | NOT  -> "NOT" 
  | LTEQ  -> "LTEQ" 
  | GTEQ  -> "GTEQ" 
  | EQ  -> "EQ" 
  | LT  -> "LT" 
  | GT  -> "GT" 
  | ARROW  -> "ARROW" 
  | EOF  -> "EOF" 
  | VAR _ -> "VAR" 
  | NUM _ -> "NUM" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | TIMES  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | POW  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | ASGN  -> (null : System.Object) 
  | THEN  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | FI  -> (null : System.Object) 
  | DO  -> (null : System.Object) 
  | OD  -> (null : System.Object) 
  | SKIP  -> (null : System.Object) 
  | IFELSE  -> (null : System.Object) 
  | LBRA  -> (null : System.Object) 
  | RBRA  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | ANDD  -> (null : System.Object) 
  | ORD  -> (null : System.Object) 
  | ANDS  -> (null : System.Object) 
  | ORS  -> (null : System.Object) 
  | NOTEQ  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | LTEQ  -> (null : System.Object) 
  | GTEQ  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | GT  -> (null : System.Object) 
  | ARROW  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | VAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NUM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 3us; 65535us; 0us; 2us; 16us; 14us; 23us; 15us; 3us; 65535us; 8us; 9us; 11us; 12us; 25us; 24us; 27us; 65535us; 5us; 6us; 8us; 36us; 11us; 36us; 17us; 18us; 20us; 21us; 25us; 36us; 43us; 26us; 44us; 27us; 45us; 28us; 46us; 29us; 47us; 30us; 48us; 31us; 49us; 32us; 52us; 33us; 53us; 34us; 55us; 35us; 65us; 36us; 66us; 36us; 67us; 36us; 68us; 36us; 69us; 36us; 70us; 37us; 71us; 38us; 72us; 39us; 73us; 40us; 74us; 41us; 75us; 42us; 9us; 65535us; 8us; 22us; 11us; 22us; 25us; 22us; 53us; 64us; 65us; 59us; 66us; 60us; 67us; 61us; 68us; 62us; 69us; 63us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 7us; 11us; 39us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 2us; 1us; 6us; 1us; 1us; 2us; 2us; 7us; 1us; 2us; 6us; 2us; 10us; 11us; 12us; 13us; 14us; 1us; 3us; 1us; 4us; 2us; 4us; 9us; 1us; 4us; 1us; 5us; 2us; 5us; 9us; 1us; 5us; 2us; 6us; 6us; 2us; 6us; 8us; 1us; 6us; 1us; 7us; 6us; 7us; 10us; 11us; 12us; 13us; 14us; 1us; 7us; 1us; 7us; 6us; 7us; 10us; 11us; 12us; 13us; 14us; 5us; 8us; 23us; 24us; 25us; 26us; 1us; 8us; 2us; 9us; 9us; 1us; 9us; 6us; 10us; 10us; 11us; 12us; 13us; 14us; 6us; 10us; 11us; 11us; 12us; 13us; 14us; 6us; 10us; 11us; 12us; 12us; 13us; 14us; 6us; 10us; 11us; 12us; 13us; 13us; 14us; 6us; 10us; 11us; 12us; 13us; 14us; 14us; 6us; 10us; 11us; 12us; 13us; 14us; 15us; 6us; 10us; 11us; 12us; 13us; 14us; 16us; 6us; 10us; 11us; 12us; 13us; 14us; 19us; 12us; 10us; 11us; 12us; 13us; 14us; 19us; 28us; 29us; 30us; 31us; 32us; 33us; 6us; 10us; 11us; 12us; 13us; 14us; 20us; 11us; 10us; 11us; 12us; 13us; 14us; 28us; 29us; 30us; 31us; 32us; 33us; 6us; 10us; 11us; 12us; 13us; 14us; 28us; 6us; 10us; 11us; 12us; 13us; 14us; 29us; 6us; 10us; 11us; 12us; 13us; 14us; 30us; 6us; 10us; 11us; 12us; 13us; 14us; 31us; 6us; 10us; 11us; 12us; 13us; 14us; 32us; 6us; 10us; 11us; 12us; 13us; 14us; 33us; 1us; 10us; 1us; 11us; 1us; 12us; 1us; 13us; 1us; 14us; 1us; 15us; 1us; 16us; 1us; 17us; 2us; 18us; 20us; 1us; 19us; 2us; 19us; 34us; 1us; 19us; 1us; 20us; 1us; 20us; 1us; 21us; 1us; 22us; 5us; 23us; 23us; 24us; 25us; 26us; 5us; 23us; 24us; 24us; 25us; 26us; 5us; 23us; 24us; 25us; 25us; 26us; 5us; 23us; 24us; 25us; 26us; 26us; 5us; 23us; 24us; 25us; 26us; 27us; 5us; 23us; 24us; 25us; 26us; 34us; 1us; 23us; 1us; 24us; 1us; 25us; 1us; 26us; 1us; 27us; 1us; 28us; 1us; 29us; 1us; 30us; 1us; 31us; 1us; 32us; 1us; 33us; 1us; 34us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 7us; 9us; 12us; 14us; 21us; 23us; 25us; 28us; 30us; 32us; 35us; 37us; 40us; 43us; 45us; 47us; 54us; 56us; 58us; 65us; 71us; 73us; 76us; 78us; 85us; 92us; 99us; 106us; 113us; 120us; 127us; 134us; 147us; 154us; 166us; 173us; 180us; 187us; 194us; 201us; 208us; 210us; 212us; 214us; 216us; 218us; 220us; 222us; 224us; 227us; 229us; 232us; 234us; 236us; 238us; 240us; 242us; 248us; 254us; 260us; 266us; 272us; 278us; 280us; 282us; 284us; 286us; 288us; 290us; 292us; 294us; 296us; 298us; 300us; |]
let _fsyacc_action_rows = 77
let _fsyacc_actionTableElements = [|4us; 32768us; 9us; 8us; 11us; 11us; 13us; 7us; 32us; 4us; 0us; 49152us; 2us; 32768us; 8us; 16us; 31us; 3us; 0us; 16385us; 2us; 32768us; 7us; 5us; 15us; 17us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 16386us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 0us; 16387us; 8us; 32768us; 2us; 48us; 3us; 49us; 5us; 53us; 17us; 57us; 18us; 58us; 24us; 69us; 32us; 51us; 33us; 50us; 2us; 32768us; 10us; 10us; 14us; 25us; 0us; 16388us; 8us; 32768us; 2us; 48us; 3us; 49us; 5us; 53us; 17us; 57us; 18us; 58us; 24us; 69us; 32us; 51us; 33us; 50us; 2us; 32768us; 12us; 13us; 14us; 25us; 0us; 16389us; 1us; 16390us; 8us; 16us; 1us; 16392us; 8us; 16us; 4us; 32768us; 9us; 8us; 11us; 11us; 13us; 7us; 32us; 4us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 6us; 32768us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 16us; 19us; 1us; 32768us; 7us; 20us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 16391us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 5us; 32768us; 19us; 67us; 20us; 68us; 21us; 65us; 22us; 66us; 30us; 23us; 4us; 32768us; 9us; 8us; 11us; 11us; 13us; 7us; 32us; 4us; 1us; 16393us; 14us; 25us; 8us; 32768us; 2us; 48us; 3us; 49us; 5us; 53us; 17us; 57us; 18us; 58us; 24us; 69us; 32us; 51us; 33us; 50us; 1us; 16394us; 4us; 47us; 1us; 16395us; 4us; 47us; 3us; 16396us; 0us; 43us; 1us; 44us; 4us; 47us; 3us; 16397us; 0us; 43us; 1us; 44us; 4us; 47us; 1us; 16398us; 4us; 47us; 3us; 16399us; 0us; 43us; 1us; 44us; 4us; 47us; 3us; 16400us; 0us; 43us; 1us; 44us; 4us; 47us; 6us; 32768us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 6us; 54us; 12us; 32768us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 6us; 54us; 23us; 71us; 25us; 75us; 26us; 73us; 27us; 70us; 28us; 74us; 29us; 72us; 6us; 32768us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 16us; 56us; 11us; 32768us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 23us; 71us; 25us; 75us; 26us; 73us; 27us; 70us; 28us; 74us; 29us; 72us; 5us; 16412us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 5us; 16413us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 5us; 16414us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 5us; 16415us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 5us; 16416us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 5us; 16417us; 0us; 43us; 1us; 44us; 2us; 45us; 3us; 46us; 4us; 47us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 0us; 16401us; 1us; 16402us; 15us; 55us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 8us; 32768us; 2us; 48us; 3us; 49us; 5us; 53us; 17us; 57us; 18us; 58us; 24us; 69us; 32us; 51us; 33us; 50us; 0us; 16403us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 0us; 16404us; 0us; 16405us; 0us; 16406us; 0us; 16407us; 2us; 16408us; 19us; 67us; 21us; 65us; 0us; 16409us; 2us; 16410us; 19us; 67us; 21us; 65us; 0us; 16411us; 5us; 32768us; 6us; 76us; 19us; 67us; 20us; 68us; 21us; 65us; 22us; 66us; 8us; 32768us; 2us; 48us; 3us; 49us; 5us; 53us; 17us; 57us; 18us; 58us; 24us; 69us; 32us; 51us; 33us; 50us; 8us; 32768us; 2us; 48us; 3us; 49us; 5us; 53us; 17us; 57us; 18us; 58us; 24us; 69us; 32us; 51us; 33us; 50us; 8us; 32768us; 2us; 48us; 3us; 49us; 5us; 53us; 17us; 57us; 18us; 58us; 24us; 69us; 32us; 51us; 33us; 50us; 8us; 32768us; 2us; 48us; 3us; 49us; 5us; 53us; 17us; 57us; 18us; 58us; 24us; 69us; 32us; 51us; 33us; 50us; 8us; 32768us; 2us; 48us; 3us; 49us; 5us; 53us; 17us; 57us; 18us; 58us; 24us; 69us; 32us; 51us; 33us; 50us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 5us; 32768us; 2us; 48us; 3us; 49us; 5us; 52us; 32us; 51us; 33us; 50us; 0us; 16418us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 5us; 6us; 9us; 10us; 13us; 19us; 25us; 26us; 35us; 38us; 39us; 48us; 51us; 52us; 54us; 56us; 61us; 67us; 74us; 76us; 82us; 88us; 94us; 99us; 101us; 110us; 112us; 114us; 118us; 122us; 124us; 128us; 132us; 139us; 152us; 159us; 171us; 177us; 183us; 189us; 195us; 201us; 207us; 213us; 219us; 225us; 231us; 237us; 243us; 249us; 250us; 252us; 258us; 267us; 268us; 274us; 275us; 276us; 277us; 278us; 281us; 282us; 285us; 286us; 292us; 301us; 310us; 319us; 328us; 337us; 343us; 349us; 355us; 361us; 367us; 373us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 3us; 1us; 3us; 3us; 3us; 6us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 2us; 2us; 1us; 1us; 3us; 4us; 1us; 1us; 3us; 3us; 3us; 3us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 16387us; 65535us; 65535us; 16388us; 65535us; 65535us; 16389us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16401us; 65535us; 65535us; 65535us; 16403us; 65535us; 16404us; 16405us; 16406us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16418us; |]
let _fsyacc_reductions ()  =    [| 
# 302 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : cmd)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 311 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : cmd)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "ProjectParser.fsp"
                                                    _1 
                   )
# 45 "ProjectParser.fsp"
                 : cmd));
# 322 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "ProjectParser.fsp"
                                                         Assignment(_1,_3) 
                   )
# 61 "ProjectParser.fsp"
                 : cmd));
# 334 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "ProjectParser.fsp"
                                                         Null 
                   )
# 62 "ProjectParser.fsp"
                 : cmd));
# 344 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : gc)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "ProjectParser.fsp"
                                                         If(_2)
                   )
# 63 "ProjectParser.fsp"
                 : cmd));
# 355 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : gc)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "ProjectParser.fsp"
                                                         Do(_2)
                   )
# 64 "ProjectParser.fsp"
                 : cmd));
# 366 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : cmd)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : cmd)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "ProjectParser.fsp"
                                                         Then(_1,_3)
                   )
# 65 "ProjectParser.fsp"
                 : cmd));
# 378 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "ProjectParser.fsp"
                                                         ArrayAs(_1,_3,_6) 
                   )
# 66 "ProjectParser.fsp"
                 : cmd));
# 391 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolean)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : cmd)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "ProjectParser.fsp"
                                                         Arrow(_1,_3) 
                   )
# 70 "ProjectParser.fsp"
                 : gc));
# 403 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : gc)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : gc)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "ProjectParser.fsp"
                                                         Ifelse(_1,_3) 
                   )
# 71 "ProjectParser.fsp"
                 : gc));
# 415 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "ProjectParser.fsp"
                                                         TimesExpr(_1,_3) 
                   )
# 76 "ProjectParser.fsp"
                 : ari));
# 427 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 77 "ProjectParser.fsp"
                                                         DivExpr(_1,_3) 
                   )
# 77 "ProjectParser.fsp"
                 : ari));
# 439 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "ProjectParser.fsp"
                                                         PlusExpr(_1,_3) 
                   )
# 78 "ProjectParser.fsp"
                 : ari));
# 451 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 79 "ProjectParser.fsp"
                                                         MinusExpr(_1,_3) 
                   )
# 79 "ProjectParser.fsp"
                 : ari));
# 463 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "ProjectParser.fsp"
                                                         PowExpr(_1,_3) 
                   )
# 80 "ProjectParser.fsp"
                 : ari));
# 475 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 81 "ProjectParser.fsp"
                                                         UPlusExpr(_2) 
                   )
# 81 "ProjectParser.fsp"
                 : ari));
# 486 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "ProjectParser.fsp"
                                                         UMinusExpr(_2) 
                   )
# 82 "ProjectParser.fsp"
                 : ari));
# 497 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : float)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 83 "ProjectParser.fsp"
                                                         Num(_1) 
                   )
# 83 "ProjectParser.fsp"
                 : ari));
# 508 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 84 "ProjectParser.fsp"
                                                         Var(_1) 
                   )
# 84 "ProjectParser.fsp"
                 : ari));
# 519 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 85 "ProjectParser.fsp"
                                                         _2 
                   )
# 85 "ProjectParser.fsp"
                 : ari));
# 530 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 86 "ProjectParser.fsp"
                                                         Array(_1,_3) 
                   )
# 86 "ProjectParser.fsp"
                 : ari));
# 542 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 90 "ProjectParser.fsp"
                                                         True 
                   )
# 90 "ProjectParser.fsp"
                 : boolean));
# 552 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 91 "ProjectParser.fsp"
                                                         False 
                   )
# 91 "ProjectParser.fsp"
                 : boolean));
# 562 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolean)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : boolean)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 92 "ProjectParser.fsp"
                                                         Ands(_1,_3) 
                   )
# 92 "ProjectParser.fsp"
                 : boolean));
# 574 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolean)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : boolean)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 93 "ProjectParser.fsp"
                                                         Ors(_1,_3) 
                   )
# 93 "ProjectParser.fsp"
                 : boolean));
# 586 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolean)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : boolean)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 94 "ProjectParser.fsp"
                                                         Andd(_1,_3) 
                   )
# 94 "ProjectParser.fsp"
                 : boolean));
# 598 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolean)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : boolean)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 95 "ProjectParser.fsp"
                                                         Ord(_1,_3) 
                   )
# 95 "ProjectParser.fsp"
                 : boolean));
# 610 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : boolean)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 96 "ProjectParser.fsp"
                                                         Not(_2) 
                   )
# 96 "ProjectParser.fsp"
                 : boolean));
# 621 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 97 "ProjectParser.fsp"
                                                         Equal(_1,_3) 
                   )
# 97 "ProjectParser.fsp"
                 : boolean));
# 633 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 98 "ProjectParser.fsp"
                                                         NotEq(_1,_3) 
                   )
# 98 "ProjectParser.fsp"
                 : boolean));
# 645 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 99 "ProjectParser.fsp"
                                                         Greater(_1,_3) 
                   )
# 99 "ProjectParser.fsp"
                 : boolean));
# 657 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 100 "ProjectParser.fsp"
                                                         GreaterEq(_1,_3) 
                   )
# 100 "ProjectParser.fsp"
                 : boolean));
# 669 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 101 "ProjectParser.fsp"
                                                         Less(_1,_3) 
                   )
# 101 "ProjectParser.fsp"
                 : boolean));
# 681 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ari)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 102 "ProjectParser.fsp"
                                                         LessEq(_1,_3) 
                   )
# 102 "ProjectParser.fsp"
                 : boolean));
# 693 "ProjectParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : boolean)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 103 "ProjectParser.fsp"
                                                         _2 
                   )
# 103 "ProjectParser.fsp"
                 : boolean));
|]
# 705 "ProjectParser.fs"
let tables () : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 37;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : cmd =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
