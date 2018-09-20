ocamlopt -c hw1.ml
ocamlopt -c hw1_tests.ml
ocamlopt -o hw hw1.cmx hw1_tests.cmx
./hw