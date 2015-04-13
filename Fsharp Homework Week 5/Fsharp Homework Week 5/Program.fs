let fst (a,b) = a;
let snd (a,b) = b;

//backwards recursion
let rec sumprod list =
    match list with
    | [] -> (0, 1)
    | x::xs -> let y = sumprod xs
               (x + (fst y), x * (snd y))

               
//QUESTION 1
//==========

//forwards recursion - I guess I just pass through a tuple
let rec fsumprod list = 
    let rec fsumprod list tuple =
        match list with
        | [] -> tuple
        | x::xs -> fsumprod xs (x + (fst tuple), x * (snd tuple))
    fsumprod list (0, 1)

//EXPERIMENTS
//===========

//quick experiments with |>
let scaledSwap d (a,b) = (d*b,d*a)
//pass the output of fsumprod to scaledswap
let scaledswappedAns b = fsumprod [1;2;3;4;5] |> scaledSwap b

//quick experiments with >>
//compose 2 functions
let coolfunc = fsumprod >> scaledSwap 2
let ans = coolfunc [1;2;3;4;5]

//quick experiments with records
type person = {name :string ; age: int}
//f# seems to infer that doug is a person based on the fields
let doug = {name="Doug"; age=21}
//let's create some ambiguity
type dog = {name :string ; age: int}
//now doug2 is a dog
let doug2 = {name="Doug"; age=21}
//we need to assert that this is in fact a person
let doug3 : person = {name="Doug"; age= 21}

//QUESTION 2
//==========
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

let classlist = [s1; s2; s3; s4; s5; s6]

//get average
let rec getAve list getValue =
    let rec fgetAve list getValue tuple =
        match list with
        | [] -> tuple
        | x::xs -> fgetAve xs getValue ((getValue x) + (fst tuple), 1.0 + (snd tuple))
    let y = fgetAve list getValue (0.0 , 0.0)
    (fst y) / (snd y)
    
let ave = getAve classlist (fun item -> item.mark)

//ranked list
let compareStudent s1 s2 = float (s1.mark - s2.mark)

let rec insertOrdered item list comparator =
    match list with
    | [] -> [item]
    | x::xs -> if (comparator item x) >= 0.0 then x::(insertOrdered item xs comparator) else item::x::xs

let insertionsort list comparefn =
    let rec finsertionsort list list2 =
        match list with
        | [] -> list2
        | x::xs -> finsertionsort xs (insertOrdered x list2 comparefn)
    finsertionsort list []

let reverselist list =
    let rec freverselist list list2 =
        match list with
        | [] -> list2
        | x::xs -> freverselist xs (x::list2)
    freverselist list []

let updateRank sortedList =
    let rec fupdaterank revSortedList list2 i =
        match revSortedList with
        | [] -> list2
        | x::xs -> fupdaterank xs ({x with rank = i}::list2) (i+1)
    fupdaterank (reverselist sortedList) [] 1

//rankedlist
let rankedlist = updateRank (insertionsort classlist compareStudent)

let one (a,b,c) = a;
let two (a,b,c) = b;
let thr (a,b,c) = c;

//summary stats
let summarystats list =
    let rec fsummarystats list tuple =
        match list with
        | [] -> tuple
        | x::xs when x.mark >= 75.0 -> fsummarystats xs ((one tuple) + 1, two tuple, thr tuple)
        | x::xs when x.mark >= 40.0 -> fsummarystats xs (one tuple, (two tuple) + 1, thr tuple)
        | x::xs -> fsummarystats xs (one tuple, two tuple, (thr tuple) + 1)
    fsummarystats list (0,0,0)