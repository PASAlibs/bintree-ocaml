(*
  Created by Antoine Girard for PASA Team
  https://github.com/PASAlibs/bintree-ocaml
  Example usage bintree lib
*)

open Bintree

let a = (createTree 5 (createTree 1 (createTree 0 Nul Nul) (createTree 4 Nul Nul)) (createTree 7 (createTree 6 Nul Nul) (createTree 9 Nul Nul)));;

let applyFunc v =
  printf "%u \n" v
;;

let _ =
  printf "%u \n" (nbLeaf a);
  printf "%u \n" (size a);
  printf "%u \n" (height a);
  printf "%u \n" (nbOccu a  1);
  printf "%u \n" (lengthDevelop a);
  prefix applyFunc a;
  if (isABR a) then
    printf "This binary tree is ABR"
  else
    printf "This binary tree is NOT ABR";
  ()
;;
