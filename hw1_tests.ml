open Hw1;;

(* is_positive: int list -> bool *)
assert(is_positive [2] = true);;
assert(is_positive [4] = true);;
assert(is_positive [4; -1] = false);;


(* is_sorted: int list -> bool *)
assert(is_sorted [] = true);;
assert(is_sorted [4] = true);;
assert(is_sorted [4; -1] = false);;

 
(*count_branches: int_tree -> int *)
assert(count_branches Lf = 0);;
assert(count_branches (Br(1, Lf, Lf)) = 1);;
assert(count_branches (Br(1, Lf, Br(3, Br(4, Lf, Lf), Lf))) = 8);;

(* depth: int_tree -> int *)
assert(depth Lf = 0);;
assert(depth (Br(1, Lf, Lf)) = 1);;
assert(depth (Br(1, Lf, Br(3, Br(4, Lf, Lf), Lf))) = 3);;


(* gen_tree: int -> int_tree *)
assert(gen_tree 0 = Lf);;
assert(gen_tree 1 = Br(1, Lf, Lf));;
assert(gen_tree 3 = Br(3, Lf, Br(2, Lf, Br(1, Lf, Lf))));;
assert(6 = count_branches (gen_tree 3));;
assert(4 = depth (gen_tree 4));;

(* inorder: int_tree -> int list *)
let big_tree = Br(6, Br(2, Br(1, Lf, Lf), Br(4, Br(3, Lf, Lf), Br(5, Lf, Lf))), Br(7, Lf, Br(9, Br(8, Lf, Lf), Lf)));;
assert(inorder(Br(1, Lf, Lf)) = [1]);;
assert(inorder big_tree = [1;2;3;4;5;6;7;8;9]);;

(* preorder: int_tree -> int list *)
assert(preorder(Br(1, Lf, Lf))  = [1]);;
assert(preorder big_tree = [6;2;1;4;3;5;7;9;8]);;


(* search_book: key -> book_type list -> book_type list *)
assert(search_book "hello" [] = []);;
let author = {fname = "Mark"; sname = "Twain"};;
let book1 = {author = author; name = "The Adventures of Tom Sawyer"; year = 1876};;
let book2 = {author = author; name = "Adventures of Huckleberry Finn "; year = 1884};;
    
let books = [book1; book2];;
assert(search_book "hello" [] = []);;
assert(List.length (search_book "Adventures" books) = 2);;
assert(List.length (search_book "Finn" books) = 1);;    


(* simplify: bool_expr - > bool_expr *)
assert(simplify(Const(True)) = Const(True));;
assert(simplify(And(Const(True), Const(False))) = Const(False));;
assert(simplify(Const(True)) = Const(True));;
assert(simplify(And(Const(True), Const(False))) = Const(False));;
assert(simplify(And(Const(True), Var("any_var"))) = Var("any_var"));;
assert(simplify(Not(And(Const(True), Var("any_var")))) = Var("any_var"));;
assert(simplify(Not(Or(Const(True), Var("any_var")))) = Const(True));;


(* lookup: string -> (string * letlang_expr) list -> letlang_expr option *)

assert((lookup "x" []) = None);;
assert((lookup "x" [("x", Const 15)]) = Some (Const 15));;
assert((lookup "x" [("x", Const 15); ("x", Const 14)]) = Some (Const 15));;

(* eval: letlang_expr -> letlang_expr *)

let e1 = Const 42;;
let e2 = Var "x";;
let e3 = Let("y", Const 15, Var "y");;
let e4 = Let("y", Const 15, Let("y", Const 14, Var "y"));;

assert((eval e1) = (Const 42));;
assert(try ignore((eval e2)); false with
        | UndefinedVariable v -> (v = "x")
        | _ -> false);;

assert((eval e3) = Const(15));;
assert((eval e4) = Const(14));;


Printf.printf "All Tests passed!\n"
