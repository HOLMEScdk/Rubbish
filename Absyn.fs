(* File Rubbbish/Absyn.fs
   Abstract syntax for the simple expression language 
 *)

module Absyn

type typ =
  | TypI                             (* Type int                    *)
  | TypC                             (* Type char                   *)
  | TypF                             (*	Type float   		        *)
  | TypS                             (* Type string                 *)
  | TypA                             (* Array type                  *)
  | TypL                             (* List type                   *)
  | TypM                             (* Map type                    *)
  | TypMat                           (* Mat type                    *)
                                                                   
and expr =                                                         
  | Access of access                 (* x    or  a[e]               *)
  | Assign of access * expr          (* x=e  or  a[e]=e             *)
  | CstI of int                      (* Constant int                *)
  | CstB of int                      (* Constant bool               *)
  | CstF of float                    (* Constant float              *)
  | CstS of string                   (* Constant string             *)
  | CstA of expr list                (* Array                       *)
  | CstL of expr list                (* List                        *)
  | Null
  | CstM of (expr * expr) list       (* Map                         *)
  | Prim1 of string * expr           (* Unary primitive operator    *)
  | Prim2 of string * expr * expr    (* Binary primitive operator   *)
  | Andalso of expr * expr           (* Sequential and              *)
  | Orelse of expr * expr            (* Sequential or               *)
  | Call of string * expr list       (* Function call f(...)        *)
                                                                   
and access =                                                       
  | AccVar of string                 (* Variable access        x    *)
  | AccIndex of access * expr        (* Array indexing         a[e] *)
                                                                   
and stmt =                                                         
  | If of (expr option * stmt) list  (* Conditional                 *)
  | Switch of expr * (expr option * stmt) list
                                     (* Switch                      *)
  | While of expr * stmt             (* While loop                  *)
  | For of string * expr * stmt      (* For loop                    *)
  | Expr of expr                     (* Expression statement   e;   *)
  | Return of expr option            (* Return from method          *)
  | Break                            (* Break from loop             *)
  | Continue                         (* Continue in loop            *)
  | Block of stmtordec list          (* Block: grouping and scope   *)
  | TryCatchFinal of stmt * stmt * stmt option
  | Import of string
  | Lambda of string list * stmt
                                                                   
and stmtordec =                                                    
  | Stmt of stmt                     (* A statement                 *)

and topdec = 
  | Fundec of string * string list * stmt
  | Main of stmtordec list

and program = 
  | Prog of topdec list
