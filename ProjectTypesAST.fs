// This file implements a module where we define a data type "expr"
// to store represent exprthmetic expressions
module ProjectTypesAST

// type expr =
//   | Num of float
//   | TimesExpr of (expr * expr)
//   | DivExpr of (expr * expr)
//   | PlusExpr of (expr * expr)
//   | MinusExpr of (expr * expr)
//   | PowExpr of (expr * expr)
//   | UPlusExpr of (expr)
//   | UMinusExpr of (expr)
//   | Array of string*expr
//   | True 
//   | False 
//   | Ands of expr * expr
//   | Ors of expr * expr
//   | Andd of expr * expr
//   | Ord of expr * expr
//   | Not of expr
//   | Equal of expr * expr 
//   | NotEq of expr * expr 
//   | Greater of expr * expr
//   | GreaterEq of expr * expr
//   | Less of expr * expr
//   | LessEq of expr * expr
//   | Arrow of expr*expr
//   | Ifelse of expr * expr
//   | Assignment of string*expr
//   | If of expr
//   | Do of expr
//   | Then of expr*expr
//   | ArrayAs of string*expr*expr
//   | Null
//   | Var of string

type cmd =
  | Assignment of string*ari
  | If of gc
  | Do of gc
  | Then of cmd*cmd
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

// type expr =
//   | Num of float
//   | TimesExpr of (ari * ari)
//   | DivExpr of (ari * ari)
//   | PlusExpr of (ari * ari)
//   | MinusExpr of (ari * ari)
//   | PowExpr of (ari * ari)
//   | UPlusExpr of (ari)
//   | UMinusExpr of (ari)
//   | Array of string*ari
//   | True of bool
//   | False of bool
//   | Ands of boolean * boolean
//   | Ors of boolean * boolean
//   | Andd of boolean * boolean
//   | Ord of boolean * boolean
//   | Not of boolean
//   | Equal of ari * ari 
//   | NotEq of ari  * ari 
//   | Greater of ari * ari 
//   | GreaterEq of ari * ari
//   | Less of ari * ari 
//   | LessEq of ari * ari
//   | Arrow of bool*cmd
//   | Ifelse of gc * gc
//   | Assignment of string*ari
//   | If of gc
//   | Do of gc
//   | Then of cmd*cmd
//   | ArrayAs of string*ari*ari