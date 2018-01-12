module RBC.GrammerAnalysis

open System.Collections.Generic
open RBCErrors
open AbstractTree
(* ArrayAssignmentExpression add three into stack (name,index,val) *)
(* declaration equals TypeSpec * Identifier which means define/describe  arr or var *)
type SymbolScope(parent : SymbolScope option) =
    let mutable list = List.empty<VariableDeclaration>    (* A list contain arr and var*)
    let identifierFromDeclaration matchDeclaration =
        match matchDeclaration with                       (* match TypeSpec(any) * Identifier *)
        | ScalarVariableDeclaration(_, i)
        | ArrayVariableDeclaration(_, i) -> i

    let declaresIdentifier (identifierRef : IdentifierRef) declaration =     (* use the type IdentifierRef *)
        (identifierFromDeclaration declaration) = identifierRef.Identifier   (* judege whether identifer equals declaration *)

    member this.AddDeclaration declaration =  (* add declaration to a List and judege whether it has apprearenced *)
        if List.exists (fun x -> identifierFromDeclaration x = identifierFromDeclaration declaration) list then
            raise (variableAlreadyDefined (identifierFromDeclaration declaration))
        list <- declaration :: list

    member this.FindDeclaration identifierRef =
        let found = List.tryFind (fun x -> declaresIdentifier identifierRef x) list    (* find if it exists ,then return declaration because using x else none*)
        match found with
        | Some(d) -> d
        | None ->       (* if not find then search its parent  *)
            match parent with 
            | Some(ss) -> ss.FindDeclaration identifierRef
            | None -> raise (nameDoesNotExist (identifierRef.Identifier))   (* Exception *)

type SymbolScopeStack() =              (* create a limited-scope stack *)
    let stack = new Stack<SymbolScope>()
    do stack.Push(new SymbolScope(None))  (* Initialization stack *)

    member this.CurrentScope = stack.Peek()

    member this.Push() = stack.Push(new SymbolScope(Some(stack.Peek())))
    member this.Pop() = stack.Pop() |> ignore
    member this.AddDeclaration declaration = stack.Peek().AddDeclaration declaration  (* to call SymbolScope's member method *)

type VariableType =
    {
        Type    : TypeSpec;
        IsArray : bool;
    }

let scalarType t = { Type = t; IsArray = false; }   (* fun define var type and set it belongs nonArray *)

type FunctionTableEntry =   (* define function Entry attributes type  *)
    {
        ReturnType     : TypeSpec;
        ParameterTypes : VariableType list;
    }

let typeOfDeclaration matchDelaration =      (* already define the var type but free to name then return its VariableType *)
    match matchDelaration with
    | AbstractTree.ScalarVariableDeclaration(t, _) -> { Type = t; IsArray = false }
    | AbstractTree.ArrayVariableDeclaration(t, _)  -> { Type = t; IsArray = true }

type FunctionTable(program) as self =   (* declare a clss inherit dictionary which maps identifer to functiontable, find all fun in this AbstractTree *)
    inherit Dictionary<Identifier, FunctionTableEntry>()

    let rec scanDeclaration  matchDeclaration =     (* if meets the request that is function and identifer is unique then add *)
        match matchDeclaration with
        | StaticVariableDeclaration(x)    -> ()
        | FunctionDeclaration(t, i, p, _) ->
            if self.ContainsKey i then
                raise (functionAlreadyDefined i)
            self.Add(i, { ReturnType = t; ParameterTypes = List.map typeOfDeclaration p; })         (* it is a fun then add its parameter list continuely rec *)

    do
        // First add built-in methods
        self.Add("readInt",  { ReturnType = Int; ParameterTypes = []; })
        self.Add("printI", { ReturnType = Void; ParameterTypes = [ { Type = Int; IsArray = false } ]; })
        self.Add("readFloat",  { ReturnType = Float; ParameterTypes = []; })
        self.Add("printF", { ReturnType = Void; ParameterTypes = [ { Type = Float; IsArray = false } ]; })
        self.Add("printCh",{ReturnType = Void; ParameterTypes = [ { Type = Char; IsArray = false } ]; })
        program |> List.iter scanDeclaration   (* add function name and its FunctionTableEntry*)

type SymbolTable(program) as self =
    inherit Dictionary<IdentifierRef, VariableDeclaration>(HashIdentity.Reference)

    let whileStatementStack = Stack<WhileStatement>()
    let symbolScopeStack = new SymbolScopeStack()  (* create a stack class *)

    let rec scanDeclaration matchDeclaration =
        match matchDeclaration with
        | StaticVariableDeclaration(x) -> symbolScopeStack.AddDeclaration x    (* put a declaration scalar or array *)
        | FunctionDeclaration(x)       -> scanFunctionDeclaration x             (* function define *)
    (* all begin from fun *)
    and scanFunctionDeclaration (functionReturnType, _, parameters, compoundStatement) =  (* Not yet define its fun naem *)
        let rec scanCompoundStatement (localDeclarations, statements) =    (*scan fun inner statement *)
            symbolScopeStack.Push()
            localDeclarations |> List.iter (fun d -> symbolScopeStack.AddDeclaration d) (* for each *)
            statements |> List.iter scanStatement
            symbolScopeStack.Pop() |> ignore

        and scanStatement matchStatement =
            match matchStatement with
            | ExpressionStatement(es) ->
                match es with
                | Expression(e) -> scanExpression e
                | Nop -> ()
            | CompoundStatement(x) -> scanCompoundStatement x
            | IfStatement(e, s1, Some(s2)) ->        (*if else and if none else*)
                scanExpression e
                scanStatement s1
                scanStatement s2
            | IfStatement(e, s1, None) ->
                scanExpression e
                scanStatement s1
            | WhileStatement(e, s) ->               (* to push e and s and after that pop it out *)
                whileStatementStack.Push (e, s)
                scanExpression e
                scanStatement s
                whileStatementStack.Pop() |> ignore
            | ReturnStatement(Some(e)) ->
                scanExpression e
            | ReturnStatement(None) ->
                if functionReturnType <> Void then
                    raise (cannotConvertType (Void.ToString()) (functionReturnType.ToString()))
            | BreakStatement ->
                if whileStatementStack.Count = 0 then        (* break can only survive in While*)
                    raise (noEnclosingLoop())

        and addIdentifierMapping identifierRef =       (* at the top of the scopestack and addinto stack use map from identifer to declaration *)
            let declaration = symbolScopeStack.CurrentScope.FindDeclaration identifierRef
            self.Add(identifierRef, declaration)

        and scanExpression matchExpression =
            match matchExpression with
            | ScalarAssignmentExpression(i, e) ->      (* has IdentifierRef(string) and Expression *)
                addIdentifierMapping i
                scanExpression e
            | ArrayAssignmentExpression(i, e1, e2) ->
                addIdentifierMapping i
                scanExpression e1
                scanExpression e2
            | BinaryExpression(e1, _, e2) ->            (* e1 || e2 && ... *)
                scanExpression e1
                scanExpression e2
            | UnaryExpression(_, e) ->
                scanExpression e
            | IdentifierExpression(i) ->
                addIdentifierMapping i
            | ArrayIdentifierExpression(i, e) ->
                addIdentifierMapping i
                scanExpression e
            | FunctionCallExpression(_, args) ->
                args |> List.iter scanExpression
            | ArraySizeExpression(i) ->
                addIdentifierMapping i
            | LiteralExpression(l) -> ()
            | ArrayAllocationExpression(_, e) ->
                scanExpression e

        symbolScopeStack.Push()
        parameters |> List.iter symbolScopeStack.AddDeclaration          (* process parameter *)
        scanCompoundStatement compoundStatement
        symbolScopeStack.Pop() |> ignore

    do program |> List.iter scanDeclaration   (* must do this get (identifer and declaration)*)

    member this.GetIdentifierTypeSpec identifierRef =                   (* from identifer get its (Type and whether*)
        typeOfDeclaration self.[identifierRef]

type ExpressionTypeDictionary(program, functionTable : FunctionTable, symbolTable : SymbolTable) as self =
    inherit Dictionary<Expression, VariableType>(HashIdentity.Reference)
    (* from symbolTable to GetIdentifierTypeSpec *)
    let rec scanDeclaration matchDeclaration =
        match matchDeclaration with
        | FunctionDeclaration(x) -> scanFunctionDeclaration x
        | _ -> ()

    and scanFunctionDeclaration (functionReturnType, _, _, compoundStatement) =
        let rec scanCompoundStatement (_, statements) =     (* do not limit localDeclarations *)
            statements |> List.iter scanStatement

        and scanStatement matchStatement =
            match matchStatement with
            | ExpressionStatement(es) ->
                match es with
                | Expression(e) -> scanExpression e |> ignore
                | Nop -> ()
            | CompoundStatement(x) -> scanCompoundStatement x
            | IfStatement(e, s1, Some(s2)) ->
                scanExpression e |> ignore
                scanStatement s1
                scanStatement s2
            | IfStatement(e, s1, None) ->
                scanExpression e |> ignore
                scanStatement s1
            | WhileStatement(e, s) ->
                scanExpression e |> ignore
                scanStatement s
            | ReturnStatement(Some(e)) ->
                let typeOfE = scanExpression e
                if typeOfE <> scalarType functionReturnType then raise (cannotConvertType (typeOfE.ToString()) (functionReturnType.ToString()))
            | _ -> ()

        and scanExpression expression = 
            let checkArrayIndexType e =
                let arrayIndexType = scanExpression e
                if arrayIndexType <> scalarType Int then
                    raise (cannotConvertType (arrayIndexType.ToString()) (Int.ToString()))

            let expressionType =     (* All non array call scalarType declare not array *) (* each match statement return a type  *)
                match expression with
                | ScalarAssignmentExpression(i, e) ->
                    let typeOfE = scanExpression e
                    let typeOfI = symbolTable.GetIdentifierTypeSpec i    (* type and if is arr, if E and I both exist then return else raise *)
                    if typeOfE <> typeOfI then raise (cannotConvertType (typeOfE.ToString()) (typeOfI.ToString()))
                    typeOfI   (* return (int x ) *)
                | ArrayAssignmentExpression(i, e1, e2) ->
                    checkArrayIndexType e1    (* LiteralExpression get index *)
                    let typeOfE2 = scanExpression e2   
                    let typeOfI = symbolTable.GetIdentifierTypeSpec i

                    if not typeOfI.IsArray then           (* meet True *)
                        raise (cannotApplyIndexing (typeOfI.ToString()))

                    if typeOfE2.IsArray then
                        raise (cannotConvertType (typeOfE2.ToString()) (typeOfI.Type.ToString()))

                    if typeOfE2.Type <> typeOfI.Type then raise (cannotConvertType (typeOfE2.ToString()) (typeOfI.Type.ToString()))

                    scalarType typeOfI.Type  (* return (void/float nonArray) *)
                | BinaryExpression(e1, op, e2) ->   (*define binary operator*)
                    let typeOfE1 = scanExpression e1
                    let typeOfE2 = scanExpression e2
                    match op with
                    | ConditionalOr | ConditionalAnd ->
                        match typeOfE1, typeOfE2 with
                        | { Type = Bool; IsArray = false; }, { Type = Bool; IsArray = false; } -> ()
                        | _ -> raise (operatorCannotBeApplied (op.ToString()) (typeOfE1.ToString()) (typeOfE2.ToString()))
                        scalarType Bool
                    | Equal | NotEqual ->
                        match typeOfE1, typeOfE2 with
                        | { Type = a; IsArray = false; }, { Type = b; IsArray = false; } when a = b && a <> Void -> ()
                        | _ -> raise (operatorCannotBeApplied (op.ToString()) (typeOfE1.ToString()) (typeOfE2.ToString()))
                        scalarType Bool
                    | LessEqual | Less | GreaterEqual | Greater ->
                        match typeOfE1, typeOfE2 with
                        | { Type = Int; IsArray = false; }, { Type = Int; IsArray = false; }
                        | { Type = Float; IsArray = false; }, { Type = Float; IsArray = false; } ->
                            ()
                        | _ -> raise (operatorCannotBeApplied (op.ToString()) (typeOfE1.ToString()) (typeOfE2.ToString()))
                        scalarType Bool
                    | Add | Subtract | Multiply | Divide | Modulus ->
                        typeOfE1
                | UnaryExpression(_, e) ->
                    scanExpression e
                | IdentifierExpression(i) ->
                    symbolTable.GetIdentifierTypeSpec i
                | ArrayIdentifierExpression(i, e) ->     (*x = arr[index]*)
                    checkArrayIndexType e
                    scalarType (symbolTable.GetIdentifierTypeSpec i).Type      
                | FunctionCallExpression(i, a) ->     (* identifer and Arguments *)
                    if not (functionTable.ContainsKey i) then
                        raise (nameDoesNotExist i)
                    let calledFunction = functionTable.[i]    (* each fun *)
                    let parameterTypes = calledFunction.ParameterTypes
                    if List.length a <> List.length parameterTypes then
                        raise (wrongNumberOfArguments i (List.length parameterTypes) (List.length a))
                    let argumentTypes = a |> List.map scanExpression  (* iter each argumentTypes *)
                    let checkTypesMatch index l r =
                        if l <> r then raise (invalidArguments i (index + 1) (l.ToString()) (r.ToString()))
                    List.iteri2 checkTypesMatch argumentTypes parameterTypes
                    scalarType calledFunction.ReturnType
                | ArraySizeExpression(i) ->    (*complete .size*)
                    scalarType Int
                | LiteralExpression(l) ->
                    match l with
                    | BoolLiteral(b)  -> scalarType Bool
                    | IntLiteral(i)   -> scalarType Int
                    | FloatLiteral(f) -> scalarType Float
                    | CharLiteral(c)  -> scalarType Char
                | ArrayAllocationExpression(t, e) ->
                    checkArrayIndexType e
                    { Type = t; IsArray = true }

            self.Add(expression, expressionType)
            expressionType 

        scanCompoundStatement compoundStatement

    do program |> List.iter scanDeclaration

type GrammerAnalysisResult =
    {
        SymbolTable     : SymbolTable;
        ExpressionTypes : ExpressionTypeDictionary;
    }
    (* create two tables *)

let analyze program =
    let symbolTable   = new SymbolTable(program)
    let functionTable = new FunctionTable(program)
    (* find whether it has main method *)
    if not (functionTable.ContainsKey "main") then
        raise (missingEntryPoint())

    let expressionTypes = new ExpressionTypeDictionary(program, functionTable, symbolTable)

    {
        SymbolTable     = symbolTable;
        ExpressionTypes = expressionTypes;
    }