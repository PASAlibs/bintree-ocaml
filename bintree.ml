(*
  Created by Antoine Girard for PASA Team
  https://github.com/PASAlibs/bintree-ocaml
  bintree lib
*)

include Printf

(* Type Binary Tree *)
type 'a tree = Nul | Node of 'a * 'a tree * 'a tree;;

(* Classical Function *)

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

let rec height a =
  match a with
  | Nul -> -1
  | Node (_,ll,lr) -> 1 + (max (height ll) (height lr))
;;

let rec nbOccu a b =
  match (a,b) with
  | (Nul,_) -> 0
  | (Node (x,ll,lr),y) when x=y  -> 1 + (nbOccu ll b) + (nbOccu lr b)
  | (Node (_,ll,lr),_) -> 0 + (nbOccu ll b) + (nbOccu lr b)
;;

let rec lengthDevelop a =
  match a with
  | Nul -> (-1)
  | Node (_,ll,Nul) -> 1 + (size ll) + (lengthDevelop ll)
  | Node (_,Nul,lr) -> 1 + (size lr) + (lengthDevelop lr)
  | Node (_,ll,lr) -> 2 + (size ll) + (lengthDevelop ll) + (size lr) + (lengthDevelop lr)
;;

let rec prefix f a =
  match a with
  | Nul -> ()
  | Node (v,ll,lr) ->
    f v;
    prefix f ll;
    prefix f lr;
    ()
;;

let rec infix f a =
  match a with
  | Nul -> ()
  | Node (v,ll,lr) ->
    infix f ll;
    f v;
    infix f lr;
    ()
;;

let rec postfix f a =
  match a with
  | Nul -> ()
  | Node (v,ll,lr) ->
    postfix f ll;
    f v;
    postfix f lr;
    ()
;;

(* ABR *)
let rec isABR a =
  match a with
  | Nul -> true
  | Node (v,Nul,Nul) -> true
  | Node (v,ll,Nul) ->
    if v<getRoot(ll) then
      ((isABR ll))
    else
      false
  | Node (v,Nul,lr) ->
    if v<getRoot(lr) then
      ((isABR lr))
    else
      false
  | Node (v,ll,lr) ->
    if v>getRoot(ll) then
      begin
        if v<getRoot(lr) then
          ((isABR ll) && (isABR lr))
        else
          false
      end
    else
     false
;;
