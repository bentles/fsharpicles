// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

//1.	A set can be represented as a list which contains no duplicates.  Create  a new data type, TSet,
// and write function to perform the following set operations: InsertElement, DeleteElement, Union,
//  Intersection, Contains, Size, Equivalence, Difference and IsSubset.  Provide a function that converts 
//  a normal list (which may contain duplicates) into a set.  
//You can implement the above methods efficiently if you keep the elements sorted.

type TSet =
    | Empty
    | Node of int * TSet

let rec InsertSorted elem set =
    match set with
        | Empty -> Node(elem, Empty)
        | Node(curr, next) when curr = elem -> Node(curr, next)
        | Node(curr, next) when curr > elem -> Node(elem, Node(curr, next))
        | Node(curr, next) -> Node(curr, (InsertSorted elem next))

let rec DeleteElem elem set =
    match set with
        | Empty -> Empty
        | Node(curr, next) when curr = elem -> next
        | Node(curr,next) -> Node(curr, DeleteElem elem next)

let rec Union set1 set2 =
    match set1 with
        | Empty -> set2
        | Node(curr, next) -> Union next (InsertSorted curr set2)

let rec Intersection set1 set2 =
    match set1 with
    | Empty -> Empty
    | Node(curr, next) -> match set2 with
                            | Empty -> Empty
                            | Node(curr2, next2) when curr2 = curr -> Node (curr, (Intersection next next2))
                            | Node(curr2, next2) when curr2 > curr -> Intersection next set2
                            | Node(curr2, next2) -> Intersection set1 next2

let rec Contains elem set =
    match set with
    | Empty -> false
    | Node(curr, next) -> if curr = elem then true else Contains elem next

let rec Size set =
    match set with
    | Empty -> 0
    | Node(curr, next) -> 1 + Size next

let rec Equiv set1 set2 =
    match (set1, set2) with
    | (Empty, Empty) -> true;
    | (Empty, Node(_,_)) -> false;
    | (Node(_,_), Empty) -> false;
    | (Node(curr, next), Node(curr2, next2)) -> if curr = curr2 then Equiv next next2 else false;

//set1 - set2
let rec Diff set1 set2 = 
    match set2 with
    | Empty -> set1
    | Node(curr, next) -> Diff (DeleteElem curr set1) next

//is set1 ⊆ set2?
let rec IsSubset set1 set2 = 
    match (set1, set2) with
    | (Empty, Empty) -> true
    | (Empty, Node(_,_)) -> true
    | (Node(_,_), Empty) -> false;
    | (Node(curr, next), Node(curr2, next2)) when curr = curr2 -> IsSubset next next2
    | (Node(curr, next), Node(curr2, next2)) when curr > curr2 -> IsSubset set1 next2
    | (Node(curr, next), Node(curr2, next2)) -> false

let rec ListToSet list =
    match list with
    | [] -> Empty
    | h::t -> InsertSorted h (ListToSet t)


//2.	Write a function that returns the height of a binary tree.

type Tree =
    | Empty
    | Node of Tree * int * Tree

let exampletree = Node(Node(Empty,1,Node(Empty, 3, Empty)),1,Node(Empty,2,Node(Node(Empty, 4, Empty), 3, Node(Empty, 5, Empty))))

let rec GetHeight tree = 
    match tree with
    | Empty -> 0
    | Node(a,b,c) -> 1 + max (GetHeight a) (GetHeight c)

let PrettyPrint tree =
    let rec PrettyPrintHelper tree precursor =
        match tree with
        | Empty -> ""
        | Node(b,a,c) -> precursor + (string a) + PrettyPrintHelper b (precursor + "--") + PrettyPrintHelper c (precursor + "--")
    PrettyPrintHelper tree "\n|"

//3.	Write a function which returns a count of the number of internal nodes in a binary tree.

let CountInternalNodes tree =
    match tree with
    | Empty -> 0
    | Node(Empty,_, Empty) -> 0
    | Node(a,_,b) -> 
    let rec CountInternalHelper tre =
        match tre with
        | Empty -> 0
        | Node (Empty,_,Empty) -> 0 //leaf
        | Node (a,b,c) -> 1 + CountInternalHelper a + CountInternalHelper c
    CountInternalHelper a + CountInternalHelper b
                    

//4.	Write a function that takes a function as input and applies that function to every element of a 
//binary tree.

let rec ApplyToTree tree func =
    match tree with
    | Empty -> Empty
    | Node(a,b,c) -> Node(ApplyToTree a func, func b, ApplyToTree c func)

//5.	Consider the following code:

type student =
    { name : string;
        mark : float;
        rank : int }

let s1 =  { name = "Rachel"; mark = 93.5; rank = 0 } 
let s2 =  { name = "Ross"; mark = 26.7; rank = 0 } 
let s3 =  { name = "Joey"; mark = 52.5; rank = 0 } 
let s4 =  { name = "Monica"; mark = 65.8; rank = 0 } 
let s5 =  { name = "Chandler"; mark = 45.8; rank = 0 } 
let s6 =  { name = "Phoebe"; mark = 78.4; rank = 0 } 



//Create a binary search tree, studTree, which contains records of type student.  The tree must be sorted
// on mark. Write functions to Insert, Delete and Search for records in the tree.  In addition, do the 
// following:

type studTree =
    | Empty
    | Node of studTree * student * studTree

let PrettyPrintStud tree =
    let lft = "┌──"
    let rt = "└──"
    let mt = "o"
    let spc = "    "
    let barspc = "│   "
    let rec PrettyPrintHelper tree precursor left =
        match (tree, left) with
        | (Empty,_) -> ""//precursor + lft + mt 
        | (Node(Empty, a, Empty),true) -> precursor + lft + (string a.name) 
        | (Node(Empty, a, Empty),_) ->    precursor + rt + (string a.name)
        | (Node(b,a,c), true) -> (PrettyPrintHelper b (precursor + spc) true) 
                                 +  precursor + lft + (string a.name) 
                                 + (PrettyPrintHelper c (precursor + barspc) false) 
        | (Node(b,a,c), _) -> (PrettyPrintHelper b (precursor + barspc) true) 
                               +  precursor + rt + (string a.name) 
                               + (PrettyPrintHelper c (precursor + spc) false)
    match tree with
    | Empty -> ""
    | Node(Empty,b,Empty) -> "\n──" + (string b.name)
    | Node(a,b,c) -> PrettyPrintHelper a "\n    " true + "\n──" + (string b.name) + PrettyPrintHelper c "\n    " false 

let smallermarkp a b = a.mark < b.mark

let rec StudTreeInsSort tree elem smallerp =
    match tree with
    | Empty -> Node(Empty, elem, Empty)
    | Node(a,b,c) when smallerp elem b -> Node(StudTreeInsSort a elem smallerp, b ,c) 
    | Node(a,b,c) -> Node(a,b,StudTreeInsSort c elem smallerp)

let getElem tree =
    match tree with
    | Node(a,b,c) -> b

//get rightmost tree with one or fewer leaves
//assuming nonempty tree
let rec RightMost tree =
    match tree with
    | Node(Empty, a, Empty) -> tree
    | Node(Empty, a, b) -> tree
    | Node(b, a, Empty) -> tree
    | Node(a,b,c) -> RightMost(c)

let rec StudTreeDel tree elem  =
    match tree with
    | Empty -> Empty
    | Node(Empty, a, Empty) -> if a = elem then Empty else tree
    | Node (Empty, a, b) -> if a = elem then StudTreeDel b elem  else Node(Empty, a, StudTreeDel b elem )
    | Node (b, a, Empty) -> if a = elem then StudTreeDel b elem  else Node(StudTreeDel b elem , a, Empty)
    | Node (a,b,c) -> if b = elem then 
                                     let rmost = getElem (RightMost a)
                                     Node(StudTreeDel a rmost , rmost, c)
                                  else Node(StudTreeDel a elem , b, StudTreeDel c elem )

let rec SearchNameByMark tree mark =
    match tree with
    | Empty -> "Not found"
    | Node(a,b,c) -> if mark = b.mark then b.name else
                     if mark < b.mark then SearchNameByMark a mark
                     else SearchNameByMark c mark

//a.	Write a function that takes as input studTree and return the average of the class marks.

//wrote this without errors first time
let AveTreeMarks tree =
    let rec CountTreeMarks tree=
        match tree with
        | Empty -> (0, 0.0)
        | Node(a,b,c) -> 
            let (lf, rt) = CountTreeMarks(a)
            let (lf1, rt1) = CountTreeMarks(c)
            (1 + lf + lf1, b.mark + rt + rt1)
    let counttotal = CountTreeMarks tree
    snd counttotal / float (fst counttotal)

//b.	Write a function that takes as input a studTree and returns a studTree in which the value of rank
// has been updated.  

//wrote this without errors first time
let RankTrav tree =
    let rec RankTraversal tree list rnk = 
        match tree with
        | Node(Empty, a, Empty) -> ({a with rank = rnk}::list, rnk + 1)
        | Node(Empty,b,c) -> let (rlist, rcnt) = RankTraversal c list rnk
                             ({b with rank = rcnt}::rlist, 1 + rcnt)   
        | Node(a,b,Empty) -> let (blist, bcnt) = ({b with rank = rnk}::list, rnk + 1)
                             RankTraversal a blist bcnt   
        | Node (a,b,c) -> let (rlist, rcnt) = RankTraversal c list rnk 
                          let (blist, bcnt) = ({b with rank = rcnt}::rlist, 1 + rcnt)   
                          RankTraversal a blist bcnt
    fst (RankTraversal tree [] 1)

//c.	Write a function that takes as input studTree and returns a 3-tuple containing the number of students
// who pass with distinction, the number of students who pass without distinction, and the number of students who fail.

let thd (_,_,a) = a

//wrote this without errors first time
let rec CountPassDistPassFail tree =
    match tree with
    | Empty -> (0,0,0)
    | Node(Empty,b,Empty) -> if b.mark >= 75.0 then (1,0,0)
                             else if b.mark >= 50.0 then (0,1,0)
                             else (0,0,1) 
    | Node(a,b,c) -> let (a1,a2,a3) = CountPassDistPassFail a
                     let (b1,b2,b3) = CountPassDistPassFail c
                     if b.mark >= 75.0 then (1 + a1 + b1, a2 + b2, a3 + b3)
                        else if b.mark >= 50.0 then (a1 + b1, a2 + b2 + 1, a3+ b3)
                        else (a1 + b1,a2 + b2, a3 + b3 + 1)
