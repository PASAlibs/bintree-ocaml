(*
  Created by Antoine Girard for PASA Team
  https://github.com/PASAlibs/bintree-ocaml
  bintree lib
*)

include Printf

type 'a tree = Nul | Node of 'a * 'a tree * 'a tree;;

let createTree v ll lr = Node (v,ll,lr);;

let getRoot a =
  match a with
  | Nul -> failwith "Tree_empty"
  | Node (v,_,_) -> v
;;

let getChildLeft a =
  match a with
  | Nul -> failwith "Tree_empty"
  | Node (_,ll,_) -> ll
;;

let getChildRight a =
  match a with
  | Nul -> failwith "Tree_empty"
  | Node (_,_,lr) -> lr
;;

let isEmpty a = a = Nul;;

let rec nbLeaf a =
  match a with
  | Nul -> 0
  | Node (_,Nul,Nul) -> 1
  | Node (_,ll,lr) -> (nbLeaf ll) + (nbLeaf lr)
;;

let rec isInTree a x =
  match a with
  | Nul -> false
  | Node (v,ll,lr) -> (v=x) || (isInTree ll x) || (isInTree lr x)
;;

let rec size a =
  match a with
  | Nul -> 0
  | Node (_,ll,lr) -> 1 + (size ll) + (size lr)
;;
