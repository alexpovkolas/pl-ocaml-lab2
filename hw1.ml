

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

(* type letlang_expr = 
  Const of int
  | Var of string
  | Let of string * letlang_expr * letlang_expr *)

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


let inorder tree =
  let rec aux tree acc = 
    match tree with
      Lf -> acc
      | Br (i, left, right) -> aux left (i::(aux right acc))
  in aux tree [];;

let preorder tree =
  let rec aux tree acc = 
    match tree with
      Lf -> acc
      | Br (i, left, right) -> i::aux left (aux right acc)
  in aux tree [];;


let check_book term book =
  let rec find list =
    match list with
    [] -> false
    | h::t -> if h = term then true else find t in

  let {author = a; name = n; year = y} = book in
  let {fname = fn; sname = sn} = a in

  find (fn::sn::(String.split_on_char ' ' n));;


let search_book term books =
  let rec aux books acc =
    match books with
      [] -> acc
      | h::t -> aux t (if check_book term h then h::acc else acc) in
  aux books [];;
         
let rec simplify expr =
  match expr with
    Const c -> expr
    | Var s -> expr
    | Not e -> simplify e    
    | And (Const(True), e) -> simplify e
    | And (e, Const(True)) -> simplify e
    | And (Const(False), e) -> Const(False)
    | And (e, Const(False)) -> Const(False)          
    | And (e1, e2) -> And(simplify e1, simplify e2) 
    | Or (Const(True), e) -> Const(True)
    | Or (e, Const(True)) -> Const(True)
    | Or (Const(False), e) -> simplify e
    | Or (e, Const(False)) -> simplify e
    | Or (e1, e2) -> Or(simplify e1, simplify e2);;








assert(simplify(Const(True)) = Const(True));;
assert(simplify(And(Const(True), Const(False))) = Const(False));;
assert(simplify(And(Const(True), Var("any_var"))) = Var("any_var"));;
assert(simplify(Not(And(Const(True), Var("any_var")))) = Var("any_var"));;
assert(simplify(Not(Or(Const(True), Var("any_var")))) = Const(True));;

let author = {fname = "Mark"; sname = "Twain"};;
let book1 = {author = author; name = "The Adventures of Tom Sawyer"; year = 1876};;
let book2 = {author = author; name = "Adventures of Huckleberry Finn "; year = 1884};;
    
let books = [book1; book2];;
assert(search_book "hello" [] = []);;
assert(check_book "Adventures" book2 = true);;    
assert(List.length (search_book "Adventures" books) = 2);;
assert(List.length (search_book "Finn" books) = 1);;    
  
let rec print_list list =
  match list with
    [] -> ()
    | e::l -> print_int e; print_string " "; print_list l;;

let big_tree = Br(6, Br(2, Br(1, Lf, Lf), Br(4, Br(3, Lf, Lf), Br(5, Lf, Lf))), Br(7, Lf, Br(9, Br(8, Lf, Lf), Lf)));;

(* print_list (preorder(big_tree));; *)

assert(preorder big_tree = [6;2;1;4;3;5;7;9;8]);;

assert(inorder(Br(1, Lf, Lf)) = [1]);;
assert(inorder big_tree = [1;2;3;4;5;6;7;8;9]);;

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