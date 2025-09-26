(******************************)
(*** For debugging purposes ***)
(******************************)

(* print out an integer list *)
let rec print_int_list lst =
  match lst with
  | [] -> ()
  | [x] -> print_int x; print_newline ()
  | x :: xs -> print_int x; print_string "; "; print_int_list xs

(* print out a string list *)
let rec print_string_list lst =
  match lst with
  | [] -> ()
  | [x] -> print_string x; print_newline ()
  | x :: xs -> print_string x; print_string "; "; print_string_list xs


(********************)
(* Problem 1: pow *)
(********************)

let rec pow x p =
  if x = 0 then 0
  else if p = 0 then 1
  else if p = 1 then x
  else x * pow x (p - 1)


(********************)
(* Problem 2: range *)
(********************)

let rec range num1 num2 =
  if num2 < num1 then []
  else num1::range (num1 + 1) num2

(**********************)
(* Problem 3: flatten *)
(**********************)

let rec flatten l =
  match l with
  | [] -> []
  | h :: t -> h @ flatten t 

(*****************************)
(* Problem 4: remove_stutter *)
(*****************************)

let rec remove_stutter l =
  match l with
  | [] -> []
  | [x] -> [x]
  | x :: y :: t -> 
      if x = y then remove_stutter (y :: t)
      else x :: remove_stutter (y :: t)

(*********************)
(* Problem 5: rotate *)
(*********************)

let rotate l n =
  let len = List.length l in
  let k = len - n in
  let rec split i lst =
    match i, lst with
    | 0, _ -> ([], lst)
    | _, [] -> ([], [])
    | i, h::t ->
        let (lf, ri) = split (i - 1) t in
        (h::lf, ri)
  in
  let (left, right) = split k l in
  right @ left 

(*******************)
(* Problem 6: jump *)
(*******************)

(*let jump lst1 lst2 =
  let index = 1 in
  let rec split i l1 l2 =
    let k = (i mod 2) in
    match k, l1, l2 with
      | (_, [], _) -> []
      | (_, _, []) -> []
      | (1, x::y, h::t) -> 
          let ele = split (i + 1) y t in
          h::ele
      | (0, h::t, x::y) ->
          let ele = split (i + 1) t y in
          h::ele
  in
  split index lst1 lst2
*)

let jump lst1 lst2 =
  let rec combine i l1 l2 =
    match (l1, l2) with
    | (_, []) | ([], _) -> []
    | (h1::t1, h2::t2) ->
        if (i + 1) mod 2 = 1
        then h2::combine (i + 1) t1 t2
        else h1::combine (i + 1) t1 t2
  in
  combine 0 lst1 lst2


(******************)
(* Problem 7: nth *)
(******************)

(*let nth l n =
  let k = n in
  let rec combine n l = 
    match (k, l) with
    | (_, []) -> []
    | (1, h::t) -> 
      let k = n in h::combine k t
    | (_, _) -> let k = k - 1 in combine n  
  in
  combine n l
*)

let nth l n =
  let rec combine i lst=
    match lst with
    | [] -> []
    | h::t ->
        if i mod n = n - 1
        then h :: combine (i + 1) t
        else combine (i + 1) t
  in
  combine 0 l

(*****************************************************)
(* Problem 8: Digital Roots and Additive Persistence *)
(*****************************************************)

(* digits : int -> int list
 * we assume n >= 0
 * (digits n) is the list of digits of n in the order in which they appear in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *)

(*let rec digitsOfInt n =
  if n < 0 then []
  else if n < 10 the [n]
  else digitsOfInt (n/10) @ [n mod 10]
*)


let rec digitsOfInt n =
  if n < 0 then []
  else
    let rec aux m acc =
      if m = 0 then acc
      else aux (m / 10) ((m mod 10) :: acc)
    in
    if n = 0 then [0] else aux n []

(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits,
 * then adding the digits of the number derived from it, etc.,
 * until the remaining number has only one digit.
 * The number of additions required to obtain a single digit from a number n
 * is called the additive persistence of n, and the digit obtained is called
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)

let rec sum lst =
  match lst with
  | [] -> 0
  | h::t -> h + sum t

let additivePersistence n =
  let rec acc m count =
    if m < 10 then count
    else
      let s = sum (digitsOfInt m) in
      acc s (count + 1)
  in
  acc n 0

let rec digitalRoot n =
  if n < 10 then n
  else digitalRoot (sum (digitsOfInt n))

(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for pow *)
  let _ =
    try
      assert (pow 3 1 = 3);
      assert (pow 3 2 = 9);
      assert (pow (-3) 3 = -27)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for range *)
  let _ =
    try
      assert (range 2 5 = [2;3;4;5]);
      assert (range 0 0 = [0])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for flatten *)
  let _ =
    try
      assert (flatten [[1;2];[3;4]] = [1;2;3;4]);
      assert (flatten [[1;2];[];[3;4];[]] = [1;2;3;4])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for remove_stutter *)
  let _ =
    try
      assert (remove_stutter [1;2;2;3;1;1;1;4;4;2;2] = [1; 2; 3; 1; 4; 2]);
      assert (remove_stutter [] = []);
      assert (remove_stutter [1;1;1;1;1] = [1]);
      assert (remove_stutter [1;1;1;1;1;2] = [1;2])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for rotate *)
  let _ =
    try
      assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 2 = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]);
      assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 0 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]);
      assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 7 = ["b"; "c"; "d"; "e"; "f"; "g"; "h"; "a"])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for jump *)
  let _ =
    try
      assert (jump ["first"; "second"; "third"; "fourth"] ["fifth"; "sixth"; "seventh"; "eighth"] = ["fifth"; "second"; "seventh"; "fourth"]);
      assert (jump [1; 3; 5; 7] [0; 2; 4; 6; 8] = [0; 3; 4; 7]);
      assert (jump ["a"; "b"] ["c"] = ["c"])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for nth *)
  let _ =
    try
      (*print_int_list (nth [1; 2; 3; 4; 5; 6; 7] 1);*)
      assert (nth [1; 2; 3; 4; 5; 6; 7] 1 = [1; 2; 3; 4; 5; 6; 7]);
      assert (nth [1; 2; 3; 4; 5; 6; 7] 2 = [2; 4; 6]);
      assert (nth [1; 2; 3; 4; 5; 6; 7] 3 = [3; 6])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for digitsOfInt *)
  let _ =
    try
      assert (digitsOfInt 3124 = [3;1;2;4]);
      assert (digitsOfInt 352663 = [3;5;2;6;6;3]);
      assert (digitsOfInt 31243 = [3;1;2;4;3]);
      assert (digitsOfInt 23422 = [2;3;4;2;2])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for additivePersistence *)
  let _ =
    try
      assert (additivePersistence 9876 = 2)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for digitalRoot *)
  let _ =
    try
      assert (digitalRoot 9876 = 3)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  Printf.printf ("%d out of 10 programming questions are correct.\n") (10 - !error_count)

let _ = main()
