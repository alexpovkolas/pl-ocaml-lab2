

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
    | h::t -> if h < 0 then false else is_positive t

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
      | Br (i, left, right) -> aux left (i::(aux right acc)) in
  aux tree [];;

let preorder tree =
  let rec aux tree acc = 
    match tree with
      Lf -> acc
      | Br (i, left, right) -> i::aux left (aux right acc) in
  aux tree [];;

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
         
let rec simplify (expr: bool_expr) =
  match expr with
    Const _ -> expr
    | Var _ -> expr
    | Not e -> simplify e    
    | And (Const(True), e) | And (e, Const(True)) -> simplify e
    | And (Const(False), _) | And (_, Const(False)) -> Const(False)
    | And (e1, e2) -> And(simplify e1, simplify e2) 
    | Or (Const(True), _) | Or (_, Const(True)) -> Const(True)
    | Or (Const(False), e) | Or (e, Const(False)) -> simplify e
    | Or (e1, e2) -> Or(simplify e1, simplify e2);;

let rec lookup v en =
  match en with 
    [] -> None
    | (name, expr)::t -> if name = v then Some expr else lookup v t;;

let eval expr =
  let rec eval_env ex env =
    match ex with
      Const _ -> ex
      | Var v -> 
        (match lookup v env with
          None -> raise (UndefinedVariable v)
          | Some e ->  eval_env e env)
      | Let (l, ex1, ex2) -> 
        let nexp = eval_env ex1 env in
        let link = (l, nexp) in
        eval_env ex2 (link::env) in
  eval_env expr [];;

