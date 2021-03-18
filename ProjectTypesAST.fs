// This file implements a module where we define a data type "expr"
// to store represent exprthmetic expressions
module ProjectTypesAST


type cmd =
  | Assignment of string*ari
  | Then of cmd*cmd
  | If of gc
  | Do of gc
  | ArrayAs of string*ari*ari
  | Null 
and ari =
  | Num of float
  | TimesExpr of (ari * ari)
  | DivExpr of (ari * ari)
  | PlusExpr of (ari * ari)
  | MinusExpr of (ari * ari)
  | PowExpr of (ari * ari)
  | UPlusExpr of (ari)
  | UMinusExpr of (ari)
  | Array of string*ari
  | Var of string
and boolean =
  | True
  | False
  | Ands of boolean * boolean
  | Ors of boolean * boolean
  | Andd of boolean * boolean
  | Ord of boolean * boolean
  | Not of boolean
  | Equal of ari * ari 
  | NotEq of ari  * ari 
  | Greater of ari * ari 
  | GreaterEq of ari * ari
  | Less of ari * ari 
  | LessEq of ari * ari
and gc = 
  | Arrow of boolean*cmd
  | Ifelse of gc * gc

type Node =
  | Cmd of cmd
  | Ari of ari
  | Boolean of boolean
  | Gc of gc;;

// Cmd(Assignment("T",Num(5.0)))