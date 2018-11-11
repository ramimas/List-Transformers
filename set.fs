// Whenever testing & the expected is an empty list, I used "let expected = ([] : int list)" for union, intersection, & difference
//                                                   I used "let expected = ([] : int list list)" for the product
// I spent a lot of my precious time wondering why it was failing, this was the only thing that worked.
// NOTE: I have used the "_" character when implementing some of my functions.
// This represents a "wild card", anything as long as a value is stored within.

module Set

#light

//
// returns true if set S is empty [], false if not
// 
let rec isEmpty S = 
  match S with
  | [] -> true
  | _  -> false

//
// returns true if x is in S, false if not
// 
let rec isMember x S = 
  match S with
  | [] -> false
  | e::rest -> if e = x then true else if e > x then false else isMember x rest

//
// returns the # of elements in the set
//
let rec size S = 
  match S with
  | [] -> 0
  | e::rest -> 1 + size rest
  
//
// inserts x into S, returning the new set S'
// 
// NOTE #1: elements are inserted in order using <.
// NOTE #2: if x exists in S, then S is returned unchanged.
// 
let rec insert x S = 
  match S with
  | [] -> [x]
  | e::rest -> if x < e then x::e::rest else if e = x then S else e::insert x rest

//
// removes x from S, returning the new set S'
// 
// NOTE: if x is not in S, then S is returned unchanged.
//
let rec remove x S = 
  match S with
  | [] -> []
  | e::rest -> if e = x then remove x rest else e::remove x rest

//
// returns true is A is a subset of B, false if not
//
let rec subset A B = 
  match A, B with
  | [], _ -> true
  | _, [] -> false
  | [A], [B] -> if A = B then true else false
  | e1::rest1, e2::rest2 -> if (if e1 = e2 then true else subset [e1] rest2) then subset rest1 B else false

// 
// returns true if A and B contain the same elements,
// false if not
let rec equivalent A B = 
  match A, B with
  | [], [] -> true
  | [], _  -> false
  | _, []  -> false
  | e::rest,B -> isMember e B && equivalent rest (remove e B)

//
// returns A union B
// 
// Example:
//   A = [1;2;3;4]
//   B = [2;5;6]
//   ==> [1;2;3;4;5;6]
//
let rec union A B = 
  match B with
  | [] -> A
  | e::rest -> union (insert e A) rest
  
//
// returns A intersect B
// 
// Example:
//   A = [1;2;3;4]
//   B = [2;4]
//   ==> [2;4]
//
let rec intersection A B =
  match A with
  | [] -> []
  | e::rest -> if isMember e B then e::intersection rest B else intersection rest B
  
//
// returns A - B
// 
// Example:
//   A = [1;2;3;4]
//   B = [2;4]
//   ==> [1;3]
//   
let rec difference A B = 
  match A, B with
  | [], _ -> []
  | _, [] -> A
  | e::rest, B -> if subset [e] B then difference rest B else e::(difference rest B)

// Recursive map helper function
// This is based on the List.Map function, works in the same way but recursive.
let rec private helper_map A B =
  match B with
  | [] -> []
  | e::rest -> A e::helper_map A rest

// 
// returns the cartesian product of A and B:
// 
// Example:
//   A = [1;2]
//   B = [3;4]
//   ==> [ [1;3]; [1;4]; [2;3]; [2;4] ]
// 
let rec product A B = 
  match A,B with
  | [], [] -> []
  | A, []  -> []
  | [], B  -> []
  | e::A, B -> (helper_map(fun n -> [e;n]) B) @ product A B

// 
// returns the set containing all possible subsets:
// 
// Example:  
//   S = [1;2;3]
//   ==> [ []; [1]; [2]; [3]; [1;2]; [1;3]; [2;3]; [1;2;3] ]
//

// I Created a helper function, based on List.Collect
// I tried to re-create the same output as given in the example
// The function returns the proper powerset, but not in the exact same order
// After many different attempts this was the closest, I could get.
// Professor H wasn't kidding when he said recursive powerset was tough
// 
// my Set.product output is [[]; [1]; [2]; [1; 2]; [3]; [1; 3]; [2; 3]; [1; 2; 3]]
let rec private collect S =
  function
  | [] -> []
  | e::rest -> S e @ collect S rest
  
let rec powerset S =
  match S with
  | [] -> [[]]
  | e::rest -> collect (fun n -> [n; e::n])(powerset rest)
  
