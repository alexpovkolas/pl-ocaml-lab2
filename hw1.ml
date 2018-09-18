

type int_tree = Lf | Br of int * int_tree * int_tree


type author_type = {fname: string; sname: string}
type book_type = {author: author_type; name: string; year: int}


type const = True | False
type bool_expr =
 | Const of const
 | Var of string
 | Not of bool_expr
 | And of bool_expr * bool_expr
 | Or of bool_expr * bool_expr

exception UndefinedVariable of string

type letlang_expr = 
  Const of int
  | Var of string
  | Let of string * letlang_expr * letlang_expr

let rec is_positive list = 
  match list with
    | [] -> true
    | h::t -> if h < 0 then false else is_positive t;;

let rec is_sorted list = 
  match list with
    | [] -> true
    | a::[] -> true    
    | a::b::t -> if a > b then false else is_sorted (b::t);;

let rec count_branches tree =  
    match tree with
      Lf -> 0
      | Br (i, left, right) -> i + count_branches left + count_branches right;;

let rec depth tree =
    match tree with
      Lf -> 0
      | Br (i, left, right) -> 1 + max (depth left) (depth right);; 

let rec gen_tree n =
  match n with
    0 -> Lf
    | k -> Br(k, Lf, gen_tree(k-1));;


assert(6 = count_branches (gen_tree 3));;
assert(4 = depth (gen_tree 4));;

assert(gen_tree 0 = Lf);;
assert(gen_tree 1 = Br(1, Lf, Lf));;
assert(gen_tree 3 = Br(3, Lf, Br(2, Lf, Br(1, Lf, Lf))));;

assert(depth Lf = 0);;
assert(depth (Br(1, Lf, Lf)) = 1);;
assert(depth (Br(1, Lf, Br(3, Br(4, Lf, Lf), Lf))) = 3);;

assert(count_branches Lf = 0);;
assert(count_branches (Br(1, Lf, Lf)) = 1);;
assert(count_branches (Br(1, Lf, Br(3, Br(4, Lf, Lf), Lf))) = 8);;

assert(is_positive [2] = true);;
assert(is_positive [4] = true);;
assert(is_positive [4; -1] = false);;

assert(is_sorted [] = true);;
assert(is_sorted [4] = true);;
assert(is_sorted [4; -1] = false);;
assert(is_sorted [3; 5; 5; 7] = true);;