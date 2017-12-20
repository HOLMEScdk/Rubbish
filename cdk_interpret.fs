module cdk
open Absyn
type InterType=
    | TypI of int
	| TypC of char
	| TypF of float
	| TypS of string
	| TypA of array
	| TypL of list

(* [(stringname * dataType),()]	*)
type 'data env = (string * 'data) list

(* paramdecs datastructure *)
type paramdecs = string list 	

(* find key-val *)
let rec lookup env x = 
    match env with
    | []         -> failwith (x + " not found")
    | (y, v)::yr -> if x=y then v else lookup yr x

(* (string * int ) list * int ([(string * int),(),], int)  use gernic*)
type valueEnv = int env * int

type funEnv = (paramdecs * stmt) env

(* the index of store（Map）*)
type address = int

(* map redefined as store *)
type store = Map<address,Interptype>

(* emptyMap *)
let emptyStore = Map.empty<address,Interptype>

let setSto (store : store) addr value = store.Add(addr, value)

(* get key return val *)
let getSto (store : store) addr = store.Item addr

let rec initSto loc n store = 
    if n=0 then store else initSto (loc+1) (n-1) (setSto(store) (loc) (TypI -999))
(* --------------------- init *)

(* bind x and v to env return 
new env called valueEnv,update store*)
let bindVar x v (env, nextloc) store : valueEnv * store = 
    let env1 = (x, nextloc) :: env 
    ((env1, nextloc + 1), setSto store nextloc v)

(*multi x and v called xs vs bind to env,iterator xs and vs and call
  binVar*)
(* Especially (env,nextloc) means ((string*int)list,int) equals valueEnv*)
let rec bindVars xs vs valueEnv store : valueEnv * store = 
    match (xs, vs) with 
    | ([], [])         -> (valueEnv, store)
    | (x1::xr, v1::vr) -> 
      let (valueEnv1, sto1) = bindVar x1 v1 valueEnv store
      bindVars xr vr valueEnv1 sto1
    | _ -> failwith "parameter/argument mismatch"  

let rec allocate (typ, x) (env0, nextloc) sto0 : valueEnv * store = 
    let (nextloc1, v, sto1) =
        printf "%A" typ
        match typ

(* let initEnvAndStore (topdecs : topdec list) : valueEnv * store = 
    let rec addv decs valueEnv store = 
        match decs with 
        | [] -> (valueEnv, store)
        | Vardec (typ, x) :: decr -> 
          let (locEnv1, sto1) = allocate (typ, x) valueEnv store
          addv decr locEnv1 funEnv sto1 
        | Fundec (_, f, xs, body) :: decr ->
          addv decr valueEnv ((f, (xs, body)) :: funEnv) store
    addv topdecs ([], 0) [] emptyStore *)

let rec exec stmt (valueEnv : valueEnv) (store : store) : store = 
    match stmt with
	(*some trouble in if*)
    | If(e, stmt1, stmt2) list -> 
      let (v, store1) = eval e valueEnv store
      if v<>0 then exec stmt1 valueEnv store1
              else exec stmt2 valueEnv store1
    | While(e, body) ->
      let rec loop store1 =
              let (v, store2) = eval e valueEnv store1
              if v<>0 then loop (exec body valueEnv store2)
                      else store2
      loop store
    | Expr e -> 
      let (_, store1) = eval e valueEnv store 
      store1 
    | Block stmts -> 
      let rec loop ss (valueEnv, store) = 
          match ss with 
          | [ ] -> store
          | s1::sr -> loop sr (stmtordec s1 valueEnv  store)
      loop stmts (valueEnv, store)
    | Continue -> 

    | Break ->

    | For(s, e, stmt1) ->

    | TryCatchFinal(stmt1, stmt2, stmt3) ->
 	
    | Return _ -> failwith "return not implemented"	

and eval e valueEnv store : int * store = 
    match e with
    | Access acc     -> let (loc, store1) = access acc valueEnv store
                        (getSto store1 loc, store1) 
    | Assign(acc, e) -> let (loc, store1) = access acc valueEnv store
                        let (res, store2) = eval e valueEnv  store1
                        (res, setSto store2 loc res) 
    | CstI i         -> (i, store)
    | Addr acc       -> access acc valueEnv  store
    | Prim1(ope, e1) ->
      let (i1, store1) = eval e1 valueEnv  store
      let res =
          match ope with
          | "not"      -> if i1=0 then 1 else 0
          | "print" -> (printf "%d" i1; i1)
          | "println" -> (printf "\n")
          | _        -> failwith ("unknown primitive " + ope)
      (res, store1) 
    | Prim2(ope, e1, e2) ->
      let (i1, store1) = eval e1 valueEnv store
      let (i2, store2) = eval e2 valueEnv store1
      let res =
          match ope with
          | "*"  -> i1 * i2
		  | "**" -> pown i1 i2
          | "+"  -> i1 + i2
          | "-"  -> i1 - i2
          | "/"  -> i1 / i2
          | "%"  -> i1 % i2
          | "==" -> if i1 =  i2 then 1 else 0
          | "!=" -> if i1 <> i2 then 1 else 0
          | "<"  -> if i1 <  i2 then 1 else 0
          | "<=" -> if i1 <= i2 then 1 else 0
          | ">=" -> if i1 >= i2 then 1 else 0
          | ">"  -> if i1 >  i2 then 1 else 0
          | _    -> failwith ("unknown primitive " + ope)
      (res, store2) 
    | And(e1, e2) -> 
      let (i1, store1) as res = eval e1 valueEnv  store
      if i1<>0 then eval e2 valueEnv  store1 else res
    | Or(e1, e2) -> 
      let (i1, store1) as res = eval e1 valueEnv  store
      if i1<>0 then res else eval e2 valueEnv  store1
    | Call(f, es) -> callfun f es valueEnv  store

(* Record the position of stack by map (store)*)
and access acc valueEnv store : int * store = 
    match acc with
	(*some trouble*)
    | AccVar x           -> (lookup (fst valueEnv) x, store)
    | AccIndex(acc, idx) -> 
      let (a, store1) = access acc valueEnv  store
      let aval = getSto store1 a
      let (i, store2) = eval idx valueEnv  store1
      (aval + i, store2) 

and evals es valueEnv store : int list * store = 
    match es with 
    | []     -> ([], store)
    | e1::er ->
      let (v1, store1) = eval e1 valueEnv  store
      let (vr, storer) = evals er valueEnv  store1 
      (v1::vr, storer) 
    
and callfun f es valueEnv  store : int * store =
    let (_, nextloc) = valueEnv
    let (paramdecs, fBody) = lookup funEnv f
    let (vs, store1) = evals es valueEnv store
    let (fBodyEnv, store2) = 
        bindVars (List.map snd paramdecs) vs (varEnv, nextloc) store1
    let store3 = exec fBody fBodyEnv store2 
    (-111, store3)	