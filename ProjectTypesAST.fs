// This file implements a module where we define a data type "expr"
// to store represent exprthmetic expressions
module ProjectTypesAST


// if x<0 -> x:=2 fi
// If(bool(ari(x,0)),)

//

type cmd =
  | Assignment of string*ari
  | Then of cmd*cmd
  | If of gc
  | Do of gc
  | ArrayAs of string*ari*ari
  | Null 
and ari =
  | Num of int
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

type ANode =
  | ActSkip of Null:cmd
  | ActAssign of Assignment:cmd
  | ActArrAssign of ArrayAs:cmd
  | ActBool of boolean

type edge = (int * ANode * int)

type memory = (Map<string,int> * Map<(string),int list>)





