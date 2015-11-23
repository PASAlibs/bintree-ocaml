(*
  Created by Antoine Girard for PASA Team
  https://github.com/PASAlibs/bintree-ocaml
  Example usage bintree lib
*)

open Bintree

let a = (createTree 1 (createTree 2 (createTree 3 Nul Nul) (createTree 4 Nul Nul)) Nul);;

let _ =
  printf "%u \n" (nbLeaf a);
  printf "%u \n" (size a);
  ()
;;
