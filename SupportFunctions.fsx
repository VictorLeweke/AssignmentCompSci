//Helper functions:
let tupleHead t = (fun (a,_) -> a) t;;
let tupleTail t = (fun (_,a) -> a) t;;


//Function for changing the value at an index of a list.
let replElemList lst index newVal = List.mapi (fun i x -> if i = index then newVal else x ) lst ;;
//Source: https://stackoverflow.com/questions/37091784/ocaml-function-replace-a-element-in-a-list


//List shuffling for the Non-deterministic interpreter: 
let rand = System.Random()
let swap (a: 'a list) x y =
    let tmp = a.[x]
    let a = replElemList a x a.[y] //can be done on a list if we use our replElemList function.
    replElemList a y tmp
    // change a list into array, shuffle an array (in-place) and change it back into a list. 
let shuffle lis : 'a list =
    let mutable newList = lis
    (List.iteri (fun i _ -> newList <- swap newList i (rand.Next(i, List.length lis))) newList)
    newList
    //idea of the program is to iterate through a list, swapping each element at least once.

//Source: http://www.fssnip.net/L/title/Array-shuffle 

//Turns a map into a pretty string :D
let neatMapString map = Map.fold (fun acc key value -> acc + (string key) + ": " + (string value) + "\n") "" map;;
