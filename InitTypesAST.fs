module InitTypesAST

type vars =
    | Comma of vars*vars
    | IArray of string*lst
    | IAssign of string*nums
and lst =
    | LstComma of nums*lst
    | LstNum of nums 
and nums =
    | UMinus of int
    | INum of int

// type initMain =
//   | Comma of initMain*initMain
//   | IAssign of string*initNums
//   | IArray of string*initNums*initNums
// and initLst =
//   | Lst of initNums*initLst
//   | LstNumba of initNums
// and initNums =
//   | UMinus of initNums
//   | Numba of float