(* File Expr/Absyn.fs
   Abstract syntax for the simple expression language 
 *)

module Absyn

type expr = 
  | CstI of int
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr
  | For of string * expr * expr

type sinstr =
  | SCstI of int                        (* push integer           *)
  | SVar of int                         (* push variable from value stack to  env stack *)
  | SAdd                                (* pop args, push sum     *)
  | SSub                                (* pop args, push diff.   *)
  | SMul                                (* pop args, push product *)
  | SPop                                (* pop value/unbind var   *)
  | SSwap;;                             (* exchange top and next  *)