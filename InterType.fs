module cdk
open Absyn

(* [(stringname * dataType),()] *)
type 'data env = (string * 'data) list

(* paramdecs datastructure *)
type paramdecs = string list  

type outSign =
  | SignBreak
  | SignContinue
  | SignReturn
  | SignOther

type inSign=
  | SignWhile
  | SignFor
  | SignSwitch
  | SignFunc
  | SignMain

type InterType=
  | TypB of bool
  | TypI of int
  | TypF of float
  | TypS of string
  | TypA of InterType list
  | TypL of InterType list
  | TypFun of paramdecs * stmt
  | Abort of string
  | TypeBreak
  | TypeContinue


(* find key-val *)
let rec lookup env x = 
    match env with
    | []         -> failwith (x + " not found")
    | (y, v)::yr -> if x=y then v else lookup yr x

(* (string * int ) list * int ([(string * int),(),], int)  use gernic*)
type varEnv = int env * int

(* the index of store（Map）*)
type address = int

(* map redefined as store *)
type store = Map<address,InterType>

(* emptyMap *)
let emptyStore = Map.empty<address,InterType>

let setSto (store : store) addr value = store.Add(addr, value)

(* get key return val *)
let getSto (store : store) addr = store.Item addr

let rec initSto loc n store = 
    if n=0 then store else initSto (loc+1) (n-1) (setSto(store) (loc) (TypI -999))
(* --------------------- init *)

(* bind x and v to env return 
new env called valueEnv,update store*)
let bindVar x v (env, nextloc) store : varEnv * store = 
    let env1 = (x, nextloc) :: env 
    ((env1, nextloc + 1), setSto store nextloc v)

(*multi x and v called xs vs bind to env,iterator xs and vs and call
  binVar*)
(* Especially (env,nextloc) means ((string*int)list,int) equals valueEnv*)
let rec bindVars xs vs varEnv store : varEnv * store = 
    match (xs, vs) with 
    | ([], [])         -> (varEnv, store)
    | (x1::xr, v1::vr) -> 
      let (varEnv1, sto1) = bindVar x1 v1 varEnv store
      bindVars xr vr varEnv1 sto1
    | _ -> failwith "parameter/argument mismatch"  

(* 
* 语句执行函数
* 功能 对不同类别的语句进行分别处理
* 传入值 stmt:stmt
*        varEnv:varEnv
*        store:store
* 返回值 store*varEnv
*)
let rec exec (stmt:stmt) (varEnv : varEnv) (store : store) (inSign:inSign): (varEnv*store*outSign*InterType option) = 
    match stmt with
    | If(list) -> //if语句处理，因为存在if-elif*-else多重可能，采用循环
        let rec loop list (varEnv : varEnv) (store : store)(inSign:inSign): (varEnv*store*outSign*InterType option)=
          match list with //list为（expr option*stmt）
          | []     -> (varEnv,store,SignOther,None)
          | s1::sr -> 
            let (expr,stmt) = s1
            match expr with
            | None -> exec stmt varEnv store inSign
            | Some(e) -> 
              let (v,varEnv1,store1) = eval e varEnv store //获得e的运算值v，并且生成新的store1
              if v<>TypB(false) then 
                    let (a,b,c,d) = exec stmt varEnv1 store1 inSign//成功则执行该语句
                    (a,b,c,d)
                else loop sr varEnv1 store1 inSign//失败则继续循环
        loop list varEnv store inSign
    | Switch(expr,list)-> //switch语句处理，依然存在多种可能性，采用循环
        let rec loop list (varEnv : varEnv) (store : store) (ismatched:bool): (varEnv * store * outSign*InterType option)=
            match list with
            | [] -> (varEnv,store,SignOther,None) //switch轮流判断结束，跳出
            | s1::sr->
              let (expr1,stmt) = s1 //expr1为case中的表达式
              match expr1 with
              | None -> exec stmt varEnv store SignSwitch//default 运算
              | Some(e) -> 
                let (v,varEnv1,store1) = eval expr varEnv store //获得expr的运算值v，并且生成新的store1,varEnv1
                let (v2,varEnv2,store2) = eval e varEnv1 store1 //获得e的运算值v，并且生成新的store2,varEnv2
                if ismatched then //如果已经匹配过了
                    let (varEnv3,store3,outSign1,InterType1) = exec stmt varEnv2 store2 SignSwitch
                    match outSign1 with
                    | SignBreak -> (varEnv3,store3,SignOther,InterType1)
                    | _->loop sr varEnv3 store3 true
                else//如果case没有匹配到
                    if v=v2 then 
                        let (varEnv3,store3,outSign1,InterType1) = exec stmt varEnv2 store2 SignSwitch //成功则执行该语句
                        match outSign1 with
                        | SignBreak -> (varEnv3,store3,SignOther,InterType1)
                        | _->loop sr varEnv3 store3 true
                    else 
                        loop sr varEnv2 store2 false//失败则继续循环
        loop list varEnv store false //一开始为false，即匹配失败
    | While(e, body) -> //循环语句处理
      let rec loop varEnv store=
        let tmp = eval e varEnv store
        let (v, varEnv2,store2) = tmp
        if v<>TypB(false) then 
            let (varEnv3,store3,outSign,InterType) = exec body varEnv2 store2 SignWhile
            match outSign with
            | SignBreak -> (varEnv3,store3,SignOther,InterType)
            | SignContinue -> loop varEnv3 store3
            | _ -> loop varEnv3 store3
          else (varEnv2,store2,SignOther,None)
      loop varEnv store
    | Expr e -> 
      let (_,varEnv1, store1) = eval e varEnv store 
      (varEnv1,store1,SignOther,None)
    | Block stmts -> 
      let rec loop ss varEnv store = 
        match ss with 
        | [ ] -> (varEnv,store,SignOther,None)
        | s1::sr -> 
          let (varEnv1,store1,outSign,InterType) = stmtordec s1 varEnv store inSign
          match outSign with
          | SignOther->loop sr varEnv1 store1
          | SignReturn -> (varEnv1,store1,SignReturn,InterType)
          | SignBreak -> (varEnv1,store1,SignBreak,None)
          | SignContinue -> (varEnv1,store1,SignContinue,None)
      let tmp = loop stmts varEnv store
      tmp
    | For(s, e, stmt1) ->failwith "return not implemented"
    | TryCatchFinal(stmt1, stmt2, stmt3) ->failwith "return not implemented"
    | Return value -> 
            match value with
            | Some(v1) ->
                let (inter1,varEnv1,store1) = eval v1 varEnv store
                (varEnv1,store1,SignReturn,Some(inter1))
            | None -> (varEnv,store,SignReturn,None)

(* 
* 表达式执行函数
* 功能 对不同类型的表达式进行处理
* 传入值 expr:expr
*        varEnv:varEnv
*        store:store
* 返回值 InterType*varEnv*store
*)
and eval (e:expr) (varEnv:varEnv) (store:store) : InterType * varEnv * store = 
    match e with
    | Access acc     -> let (loc, store1) = access acc varEnv store //获得变量地址
                        (getSto store1 loc, varEnv,store1) //访问变量的地址，以获得值
    | Assign(acc, e) -> try
                            let (loc, store1) = access acc varEnv store //获得左值的地址
                            let (res, varEnv2 ,store2) = eval e varEnv  store1 //对右值进行运算
                            (res, varEnv2,setSto store2 loc res)     //将左值地址对应到右值之中
                        with
                            | q -> 
                                let (list,loc) = varEnv
                                match acc with
                                | AccVar x ->
                                    let varEnv2 = ((x,loc)::list,loc+1)
                                    let (res,varEnv3,store3) = eval e varEnv2 store
                                    (res,varEnv3,(setSto store3 loc res))
    | CstI i         -> (TypI i, varEnv,store)
    | CstF f         -> (TypF f, varEnv,store)
    | CstS s         -> (TypS s, varEnv,store)
    | CstB b         -> match b with
                        | 0 -> (TypB false,varEnv,store)
                        | _ -> (TypB true,varEnv,store)
    | Tuple t        -> 
        let rec loop items list1 (varEnv:varEnv) (store:store) = 
            match items with
            | [] -> (list1,varEnv,store)
            | ti::tl -> 
                let (result,varEnv1,store1) = eval ti varEnv store //对tuple中元素进行运算，并且更新环境
                loop tl (list1@[result]) varEnv1 store1
        let(result,varEnv2,store2) = loop t [] varEnv store
        (TypL result,varEnv2,store2)
    | Array t        -> 
        let rec loop items list varEnv store = 
            match items with
            | [] -> (list,varEnv,store)
            | ti::tl -> 
                let (result,varEnv1,store1) = eval ti varEnv store//对array中元素进行运算，并且更新环境
                loop tl (list@[result]) varEnv1 store1
        let(result,varEnv2,store2) = loop t [] varEnv store
        (TypA result,varEnv2,store2)
    | Prim1(ope, e1) ->
      let (i1, varEnv1,store1) = eval e1 varEnv  store
      let res =
          match ope with
          | "not"      ->
              match i1 with
              | TypI ii1 -> if ii1=0 then TypI 1 else TypI 0 
              | TypF ii1 -> if ii1=0.0 then TypI 1 else TypI 0
              | TypS ii1 -> if ii1="" then TypI 1 else TypI 0
              | TypB ii1 -> if ii1=true then TypB false else TypB true
          | "print" -> 
              let rec loop1 (InterType:InterType)= 
                  match InterType with
                  | TypI ii1 -> (printf "%d" ii1)
                  | TypF ii1 -> (printf "%f" ii1)
                  | TypS ii1 -> (printf "%s" ii1)
                  | TypB ii1 -> (printf "%b" ii1)
                  | TypL ii1 -> 
                        let rec loop2 list= 
                            match list with
                            | [] -> ()
                            | ti::tl -> 
                                loop1 ti
                                printf "%s" ","
                                loop2 tl
                        printf "%s" "("
                        loop2 ii1
                        (printf "%s" ")")
                  | TypA ii1 -> 
                        let rec loop2 list= 
                            match list with
                            | [] -> ()
                            | ti::tl -> 
                                loop1 ti
                                printf "%s" ","
                                loop2 tl
                        printf "%s" "["
                        loop2 ii1
                        (printf "%s" "]")
              loop1 i1
              i1
          | "println" -> (printf "\n";TypI 10)
          | _        -> failwith ("unknown primitive " + ope)
      (res,varEnv1,store1) 
    | Prim2(ope, e1, e2) ->
      let (i1, varEnv1,store1) = eval e1 varEnv store
      let (i2, varEnv2,store2) = eval e2 varEnv1 store1
      let res =
          match ope with
          | "*"  -> 
            match i1 with
            | TypI(ii1) -> match i2 with
                            | TypI(ii2) -> TypI(ii1*ii2)
                            | TypF(ii2) -> TypF(float(ii1)*ii2)
                            | TypS(ii2) -> let mutable str = ""
                                           for x = 1 to ii1 do str<-str+ii2 done
                                           TypS(str)
            | TypF(ii1) -> match i2 with
                            | TypI(ii2) -> TypF(ii1*float(ii2))
                            | TypF(ii2) -> TypF((ii1)*ii2)
            | TypS(ii1) -> match i2 with
                            | TypI(ii2) -> let mutable str = ""
                                           for x = 1 to ii2 do str<-str+ii1 done
                                           TypS(str)
          | "**" -> 
            match i1 with
            | TypI(ii1) -> match i2 with
                            | TypI(ii2) -> TypI(pown ii1 ii2)
            | TypF(ii1) -> match i2 with
                            | TypI(ii2) -> TypF(pown ii1 ii2)
          | "+"  ->
              match i1 with
                | TypI(ii1) -> match i2 with
                                | TypI(ii2) -> TypI(ii1+ii2)
                                | TypF(ii2) -> TypF((float(ii1)+ii2))
                                | TypS(ii2) -> TypS(string(ii1)+ii2)
                | TypF(ii1) -> match i2 with
                                | TypI(ii2) -> TypF(ii1+float(ii2))
                                | TypF(ii2) -> TypF(ii1+ii2)
                                | TypS(ii2) -> TypS(string(ii1)+ii2)
                | TypS(ii1) -> match i2 with
                                | TypI(ii2) -> TypS(ii1+string(ii2))
                                | TypF(ii2) -> TypS(ii1+string(ii2))
                                | TypS(ii2) -> TypS(ii1+ii2)
          | "-"  -> 
              match i1 with
                | TypI(ii1) -> match i2 with
                                | TypI(ii2) -> TypI(ii1-ii2)
                                | TypF(ii2) -> TypF((float(ii1)-ii2))
                | TypF(ii1) -> match i2 with
                                | TypI(ii2) -> TypF(ii1-float(ii2))
                                | TypF(ii2) -> TypF(ii1-ii2)
          | "/"  ->
              match i1 with
                    | TypI(ii1) -> match i2 with
                                    | TypI(ii2) -> TypI(ii1/ii2)
                                    | TypF(ii2) -> TypF((float(ii1)/ii2))
                    | TypF(ii1) -> match i2 with
                                    | TypI(ii2) -> TypF(ii1/float(ii2))
                                    | TypF(ii2) -> TypF(ii1/ii2)
          | "%"  ->
              match i1 with
                    | TypI(ii1) -> match i2 with
                                    | TypI(ii2) -> TypI(ii1%ii2)
                                    | TypF(ii2) -> TypF((float(ii1)%ii2))
                    | TypF(ii1) -> match i2 with
                                    | TypI(ii2) -> TypF(ii1%float(ii2))
                                    | TypF(ii2) -> TypF(ii1%ii2)
          | "==" -> 
              match i1 with
                    | TypI(ii1) -> match i2 with
                                    | TypI(ii2) -> if ii1=ii2 then TypB(true) else TypB(false)
                                    | _ -> TypB(false)
                    | TypF(ii1) -> match i2 with
                                    | TypF(ii2) -> if ii1=ii2 then TypB(true) else TypB(false)
                                    | _ -> TypB(false)
                    | TypS(ii1) ->match i2 with
                                    | TypS(ii2) -> if ii1=ii2 then TypB(true) else TypB(false)
                                    | _ -> TypB(false)
          | "!=" -> 
              match i1 with
                    | TypI(ii1) -> match i2 with
                                    | TypI(ii2) -> if ii1=ii2 then TypB(false) else TypB(true)
                                    | _ -> TypB(true)
                    | TypF(ii1) -> match i2 with
                                    | TypF(ii2) -> if ii1=ii2 then TypB(false) else TypB(true)
                                    | _ -> TypB(true)
                    | TypS(ii1) ->match i2 with
                                    | TypS(ii2) -> if ii1=ii2 then TypB(false) else TypB(true)
                                    | _ -> TypB(true)
          | "<"  -> 
              match i1 with
                    | TypI(ii1) -> match i2 with
                                    | TypI(ii2) -> if ii1<ii2 then TypB(true) else TypB(false)
                                    | TypF(ii2) -> if float(ii1)<ii2 then TypB(true) else TypB(false)
                    | TypF(ii1) -> match i2 with
                                    | TypI(ii2) -> if ii1<float(ii2) then TypB(true) else TypB(false)
                                    | TypF(ii2) -> if ii1<ii2 then TypB(false) else TypB(true)
                    | TypS(ii1) ->match i2 with
                                    | TypS(ii2) -> if ii1<ii2 then TypB(false) else TypB(true)
          | "<=" ->
              match i1 with
                    | TypI(ii1) -> match i2 with
                                    | TypI(ii2) -> if ii1<=ii2 then TypB(true) else TypB(false)
                                    | TypF(ii2) -> if float(ii1)<=ii2 then TypB(true) else TypB(false)
                    | TypF(ii1) -> match i2 with
                                    | TypI(ii2) -> if ii1<=float(ii2) then TypB(true) else TypB(false)
                                    | TypF(ii2) -> if ii1<=ii2 then TypB(false) else TypB(true)
                    | TypS(ii1) ->match i2 with
                                    | TypS(ii2) -> if ii1<=ii2 then TypB(false) else TypB(true)
          | ">=" -> 
              match i1 with
                    | TypI(ii1) -> match i2 with
                                    | TypI(ii2) -> if ii1>=ii2 then TypB(true) else TypB(false)
                                    | TypF(ii2) -> if float(ii1)>=ii2 then TypB(true) else TypB(false)
                    | TypF(ii1) -> match i2 with
                                    | TypI(ii2) -> if ii1>=float(ii2) then TypB(true) else TypB(false)
                                    | TypF(ii2) -> if ii1>=ii2 then TypB(false) else TypB(true)
                    | TypS(ii1) ->match i2 with
                                    | TypS(ii2) -> if ii1>=ii2 then TypB(false) else TypB(true)
          | ">"  -> 
              match i1 with
                    | TypI(ii1) -> match i2 with
                                    | TypI(ii2) -> if ii1>ii2 then TypB(true) else TypB(false)
                                    | TypF(ii2) -> if float(ii1)>ii2 then TypB(true) else TypB(false)
                    | TypF(ii1) -> match i2 with
                                    | TypI(ii2) -> if ii1>float(ii2) then TypB(true) else TypB(false)
                                    | TypF(ii2) -> if ii1>ii2 then TypB(false) else TypB(true)
                    | TypS(ii1) ->match i2 with
                                    | TypS(ii2) -> if ii1>ii2 then TypB(false) else TypB(true)
          | _    -> failwith ("unknown primitive " + ope)
      (res, varEnv2,store2) 
    | And(e1, e2) -> 
      let (i1, varEnv1,store1) as res = eval e1 varEnv  store
      if i1<>TypB(false) then eval e2 varEnv1  store1 else res
    | Or(e1, e2) -> 
      let (i1, varEnv1,store1) as res = eval e1 varEnv  store
      if i1<>TypB(false) then res else eval e2 varEnv1  store1
    | Call(f, es) -> callfun f es varEnv  store

(* Record the position of stack by map (store)*)
and access acc (varEnv:varEnv) (store:store) : address * store =  //去访问变量，获得对应地址关系
    match acc with
    | AccVar x           -> (lookup (fst varEnv) x, store)
    | AccIndex(acc, idx) ->  failwith("pass")
    (*  let (a, store1) = access acc varEnv store
      let aval = getSto store1 a
      let (i, store2) = eval idx varEnv store1
      match aval with
      | TypI(aa) -> match i with
                    | TypI(ii) -> (aa + ii, store2) *)
      

and evals es varEnv store : InterType list * varEnv * store = 
    match es with 
    | []     -> ([], varEnv,store)
    | e1::er ->
      let (v1, varEnv1,store1) = eval e1 varEnv  store
      let (vr, varEnv2,storer) = evals er varEnv1  store1 
      (v1::vr,varEnv2, storer) 
    
and callfun f es varEnv  store : InterType * varEnv*store =
    let (varEnvlist, nextloc) = varEnv
    let result = lookup varEnvlist f
    let resultTyp = getSto store result
    match resultTyp with
    | TypFun(paramdecs,fBody) -> 
        let (vs, varEnv1,store1) = evals es varEnv store
        let (fBodyEnv, store2) = bindVars paramdecs vs (varEnvlist, nextloc) store1
        let storeandEnv = exec fBody fBodyEnv store2 SignFunc
        let (varEnv2,store3,a,b) = storeandEnv
        match b with
        | Some(e1) -> 
            (e1,varEnv,store)
        | None -> 
            (TypS "",varEnv,store)
    | _-> failwith(f+" is not a function")
(*
*   将新的函数加入环境
*   传入    name          //函数名称
*           paramdecs     //函数参数列表
*           stmt          //函数内嵌语句
*           varEnv  :varEnv //变量和地址关系
*           store   :store
*   返回    varEnv,store
*)
and allocatefun name (paramdecs:string list) stmt varEnv store =
    let (list,loc) = varEnv  //读取变量存放列list和下一个地址loc
    let varEnv1 = ((name,loc)::list,loc+1) //将函数名放入变量存放列中，并且地址加一
    let store1 = setSto store loc (TypFun(paramdecs,stmt))  //将地址和值对应起来
    (varEnv1,store1)

(*
*   根据传入的不同stmtordec进行操作
*   如果传入Stmt则进行执行运算 并且返回对应的环境和地址关系
*   如果传入Fundec则将Fun加入到变量环境之中 并且返回对应的环境和地址关系
*   传入    stmtordec          //语句值--Stmt或Fundec
*           varEnv  :varEnv
*           store   :store
*           inSign : inSign
*   返回    varEnv,store.outSign
*)
and stmtordec stmtordec (varEnv : varEnv) (store : store) inSign:varEnv*store*outSign*InterType option = 
    match stmtordec with 
    | Stmt(stmt)   -> 
        match stmt with
        | Break -> 
            match inSign with
            | SignWhile -> (varEnv,store,SignBreak,None)
            | SignFor -> (varEnv,store,SignBreak,None)
            | SignSwitch -> (varEnv,store,SignBreak,None)
            | _ -> failwith ("Can't break without in loop or switch")  //while For Switch里面才可以使用break
        | Continue ->
            match inSign with
            | SignWhile -> (varEnv,store,SignContinue,None)
            | SignFor -> (varEnv,store,SignContinue,None)
            | _ -> failwith ("Can't continue without in loop") //while For 里面才可以使用Continue
        | Return(e) ->
            match inSign with
            | SignFunc -> 
                let (varEnv,store,outSign,InterType) = exec stmt varEnv store inSign 
                (varEnv,store,SignReturn,InterType)                  //函数遇到return 进行exec操作,此句作用仅判断是否在函数中
            | _->failwith ("Can't return without in func") //函数里面才可以使用return
        | _->
            let (varEnv,store,outSign,InterType) = exec stmt varEnv store inSign
            (varEnv,store,outSign,InterType)
    | Fundec(name,paramdecs,stmt) -> 
        match name with
        | Some(e) ->
            let (varEnv,store) = allocatefun e paramdecs stmt varEnv store
            (varEnv,store,SignOther,None)
        | _ -> failwith "none"

(*
*   循环实现exectopdecs并返回
*   传入    varEnv  :varEnv
*           store   :store
*   返回    varEnv,store
*)
let rec exectopdecs topdecs (varEnv : varEnv) (store : store):varEnv*store= 
    let rec loop ss varEnv store = 
        match ss with 
        | [ ] -> (varEnv,store)
        | s1::sr -> 
            let (varEnv1,store1,_,_) = stmtordec s1 varEnv store SignMain //此处放入SignMain已标识当前环境为Main中,只取varEnv,store
            loop sr varEnv1 store1
    loop topdecs varEnv store

(*基本函数*)
let run (Prog topdecs) = 
    let rec loop topdecs=
        match topdecs with
        | [] -> ()
        | ti::tl -> 
            match ti with
            | Main(ii) ->exectopdecs ii ([], 0) emptyStore
            loop tl
    loop topdecs
    
    


(* Example programs are found in the files ex1.c, ex2.c, etc *)