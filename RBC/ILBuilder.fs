namespace MiniC.Compiler

open System.Collections.Generic
open SemanticAnalysis
open IL
type ILVariableScope =
    | FieldScope of ILVariable
    | ArgumentScope of int16
    | LocalScope of int16

type VariableMappingDictionary = Dictionary<AbstractTree.VariableDeclaration, ILVariableScope>

module ILBuilderUtilities =    (* define ILBuilder's type as follows some example*)
    let typeOf =
        function
        | AbstractTree.Void  -> typeof<System.Void>
        | AbstractTree.Bool  -> typeof<bool>
        | AbstractTree.Int   -> typeof<int>
        | AbstractTree.Float -> typeof<float>

    let createILVariable =
        function
        | AbstractTree.ScalarVariableDeclaration(t, i) as d ->
            {
                ILVariable.Type = typeOf t; 
                Name = i;
            }
        | AbstractTree.ArrayVariableDeclaration(t, i) as d ->
            {
                ILVariable.Type = (typeOf t).MakeArrayType(); 
                Name = i;
            }

open ILBuilderUtilities

type ILMethodBuilder(semanticAnalysisResult : SemanticAnalysisResult,
                     variableMappings : VariableMappingDictionary) =
    let mutable argumentIndex = 0s
    let mutable localIndex = 0s
    let arrayAssignmentLocals = Dictionary<AbstractTree.Expression, int16>()
    let mutable labelIndex = 0
    let currentWhileStatementEndLabel = Stack<ILLabel>()

    let lookupILVariableScope identifierRef =    (* use the identifier to find its variable field(ILVariableScope)*)
        let declaration = semanticAnalysisResult.SymbolTable.[identifierRef]
        variableMappings.[declaration]  (* get its ILVariableScope *)

    let makeLabel() =
        let result = labelIndex
        labelIndex <- labelIndex + 1
        result          (* return index *)

    let rec processBinaryExpression =                     (* process a = b || c*)
        function
        | (l, AbstractTree.ConditionalOr, r) ->
            let leftIsFalseLabel = makeLabel()            
            let endLabel = makeLabel()
            List.concat [ processExpression l                (* evaluate lhs,if is false,branch to leftIsFalseLabel,if true push into statck*)
                          [ Brfalse leftIsFalseLabel ]        
                          [ Ldc_I4 1 ]
                          [ Br endLabel ]
                          [ Label leftIsFalseLabel ]
                          processExpression r
                          [ Label endLabel ] ]
        | (l, AbstractTree.ConditionalAnd, r) ->
            let leftIsTrueLabel = makeLabel()
            let endLabel = makeLabel()
            List.concat [ processExpression l
                          [ Brtrue leftIsTrueLabel ]        (* if true turn leftIstrueLabel *)
                          [ Ldc_I4 0 ]
                          [ Br endLabel ]
                          [ Label leftIsTrueLabel ]
                          processExpression r
                          [ Label endLabel ] ]
        | (l, op, r) -> List.concat [ (processExpression l);
                                      (processExpression r);
                                      [ processBinaryOperator op ] ]

    and processBinaryOperator = (* from AbstractTree to MsIL*)     
        function
        | AbstractTree.Add -> Add
        | AbstractTree.Divide -> Div
        | AbstractTree.Multiply -> Mul
        | AbstractTree.Modulus -> Rem
        | AbstractTree.Subtract -> Sub
        | AbstractTree.Equal -> Ceq
        | AbstractTree.Greater -> Cgt
        | AbstractTree.GreaterEqual -> Cge
        | AbstractTree.Less -> Clt
        | AbstractTree.LessEqual -> Cle
        | _ -> failwith "Shouldn't be here"

(* get its ILVariableScope *)
    and processIdentifierLoad identifierRef =     (* Load the identifier *)
        match lookupILVariableScope identifierRef with
        | FieldScope(v)    -> [ Ldsfld v ]
        | ArgumentScope(i) -> [ Ldarg i ]
        | LocalScope(i)    -> [ Ldloc i ]

    and processIdentifierStore identifierRef =    (* Store the identifier *)
        match lookupILVariableScope identifierRef with
        | FieldScope(v)    -> [ Stsfld v ]
        | ArgumentScope(i) -> [ Starg i ]
        | LocalScope(i)    -> [ Stloc i ]

    and processExpression expression =     (*采用递归转化表示、ensure the top of the stack is the value of expression*)
        match expression with
        | AbstractTree.ScalarAssignmentExpression(i, e) ->
            List.concat [ processExpression e
                          [ Dup ]            (* make it double *)
                          processIdentifierStore i ]   (* pop one and remain one *)
        | AbstractTree.ArrayAssignmentExpression(i, e1, e2) as ae ->
            List.concat [ processIdentifierLoad i
                          processExpression e1
                          processExpression e2
                          [ Dup ]
                          [ Stloc arrayAssignmentLocals.[ae] ]
                          [ Stelem (typeOf (semanticAnalysisResult.SymbolTable.GetIdentifierTypeSpec i).Type) ]
                          [ Ldloc arrayAssignmentLocals.[ae] ] ]
        | AbstractTree.BinaryExpression(a, b, c) -> processBinaryExpression (a, b, c)
        | AbstractTree.UnaryExpression(op, e) ->
            List.concat [ processExpression e
                          processUnaryOperator op]
        | AbstractTree.IdentifierExpression(i) -> processIdentifierLoad i
        | AbstractTree.ArrayIdentifierExpression(i, e) ->  (*1. push the arr to stack *)
            List.concat [ processIdentifierLoad i  (*2. push the index into stack*)
                          processExpression e      (*3.call ldelem pop array and index and then push its value a[index]*)
                          [ Ldelem (typeOf (semanticAnalysisResult.SymbolTable.GetIdentifierTypeSpec i).Type) ] ]
        | AbstractTree.FunctionCallExpression(i, a) ->
            List.concat [ a |> List.collect processExpression
                          [ Call i ] ]
        | AbstractTree.ArraySizeExpression(i) ->   (* first push array and call Ldlen then pop array and get its size *)
            List.concat [ processIdentifierLoad i
                          [ Ldlen ] ]
        | AbstractTree.LiteralExpression(l) ->
            match l with
            | AbstractTree.IntLiteral(x)   -> [ Ldc_I4(x) ]
            | AbstractTree.FloatLiteral(x) -> [ Ldc_R8(x) ]
            | AbstractTree.BoolLiteral(x)  -> [ (if x then Ldc_I4(1) else Ldc_I4(0)) ]
        | AbstractTree.ArrayAllocationExpression(t, e) ->
            List.concat [ processExpression e        (* get the new array size and then create it *)
                          [ Newarr (typeOf t) ] ]

    and processUnaryOperator =       (* linke binary operator, unit expression first push operation tos stack*)
        function
        | AbstractTree.LogicalNegate -> [ Ldc_I4 0; Ceq ]
        | AbstractTree.Negate        -> [ Neg ]
        | AbstractTree.Identity      -> [ ]
     (* above expression map into IL *)
    (* below statement map into IL *)
    and processStatement =
        function
        | AbstractTree.ExpressionStatement(x) ->
            match x with
            | AbstractTree.Expression(x) ->
                let isNotVoid = semanticAnalysisResult.ExpressionTypes.[x].Type <> AbstractTree.Void
                List.concat [ processExpression x
                              (if isNotVoid then [ Pop ] else []) ]
                
            | AbstractTree.Nop -> []
        | AbstractTree.CompoundStatement(_, s) -> s |> List.collect processStatement  (* recursive *)
        | AbstractTree.IfStatement(e, s1, Some(s2)) -> (* own else *)
            let thenLabel = makeLabel()
            let endLabel = makeLabel()
            List.concat [ processExpression e
                          [ Brtrue thenLabel ]
                          processStatement s2
                          [ Br endLabel ]
                          [ Label thenLabel ]
                          processStatement s1
                          [ Label endLabel ] ]
        | AbstractTree.IfStatement(e, s1, None) ->
            let thenLabel = makeLabel()
            let endLabel = makeLabel()
            List.concat [ processExpression e
                          [ Brtrue thenLabel ]
                          [ Br endLabel ]
                          [ Label thenLabel ]
                          processStatement s1
                          [ Label endLabel ] ]
        | AbstractTree.WhileStatement(e, s) ->
            let startLabel = makeLabel()
            let conditionLabel = makeLabel()
            let endLabel = makeLabel()
            currentWhileStatementEndLabel.Push endLabel
            let result = List.concat [ [ Br conditionLabel ]
                                       [ Label startLabel ]
                                       processStatement s
                                       [ Label conditionLabel ]
                                       processExpression e
                                       [ Brtrue startLabel ]
                                       [ Label endLabel ] ]
            currentWhileStatementEndLabel.Pop() |> ignore
            result
        | AbstractTree.ReturnStatement(x) ->
            match x with
            | Some(x) -> (processExpression x) @ [ Ret ]   (* if has value in retunr push it into stack and call ret*)
            | None    -> [ Ret ]  
        | AbstractTree.BreakStatement ->
            [ Br (currentWhileStatementEndLabel.Peek()) ]

    let processVariableDeclaration (mutableIndex : byref<_>) f d =
        let v = createILVariable d
        variableMappings.Add(d, f mutableIndex)
        mutableIndex <- mutableIndex + 1s
        v

    let processLocalDeclaration declaration =
        processVariableDeclaration &localIndex (fun i -> LocalScope i) declaration
    let processParameter declaration =
        processVariableDeclaration &argumentIndex (fun i -> ArgumentScope i) declaration

    let rec collectLocalDeclarations statement =
        let rec fromStatement =
            function
            | AbstractTree.ExpressionStatement(es) ->
                match es with
                | AbstractTree.Expression(e) -> fromExpression e
                | AbstractTree.Nop -> []
            | AbstractTree.CompoundStatement(localDeclarations, statements) ->
                 List.concat [ localDeclarations |> List.map processLocalDeclaration;
                               statements |> List.collect collectLocalDeclarations ]
            | AbstractTree.IfStatement(e, s1, Some(s2)) ->
                List.concat [ fromExpression e
                              collectLocalDeclarations s1
                              collectLocalDeclarations s2 ]
            | AbstractTree.IfStatement(e, s1, None) ->
                List.concat [ fromExpression e
                              collectLocalDeclarations s1 ]
            | AbstractTree.WhileStatement(e, s) ->
                List.concat [ fromExpression e
                              collectLocalDeclarations s ]
            | AbstractTree.ReturnStatement(Some(e)) ->
                List.concat [ fromExpression e ]
            | _ -> []

        and fromExpression =
            function
            | AbstractTree.ScalarAssignmentExpression(i, e) -> fromExpression e
            | AbstractTree.ArrayAssignmentExpression(i, e1, e2) as ae ->
                let v = {
                    ILVariable.Type = typeOf ((semanticAnalysisResult.SymbolTable.GetIdentifierTypeSpec i).Type); 
                    Name = "ArrayAssignmentTemp" + string localIndex;
                }
                arrayAssignmentLocals.Add(ae, localIndex);
                localIndex <- localIndex + 1s
                List.concat [ [ v ]; fromExpression e2 ]
            | AbstractTree.BinaryExpression(l, op, r)      -> List.concat [ fromExpression l; fromExpression r; ]
            | AbstractTree.UnaryExpression(op, e)          -> fromExpression e
            | AbstractTree.ArrayIdentifierExpression(i, e) -> fromExpression e
            | AbstractTree.FunctionCallExpression(i, a)    -> a |> List.collect fromExpression
            | AbstractTree.ArrayAllocationExpression(t, e) -> fromExpression e
            | _ -> []

        fromStatement statement

    member x.BuildMethod(returnType, name, parameters, (localDeclarations, statements)) =
        {
            Name       = name;
            ReturnType = typeOf returnType;
            Parameters = parameters |> List.map processParameter;
            Locals     = List.concat [ localDeclarations |> List.map processLocalDeclaration;
                                       statements |> List.collect collectLocalDeclarations ]
            Body       = statements |> List.collect processStatement;
        }

type ILBuilder(semanticAnalysisResult) =
    let variableMappings = new VariableMappingDictionary(HashIdentity.Reference)

    let processStaticVariableDeclaration d =
        let v = createILVariable d
        variableMappings.Add(d, FieldScope(v))   (*AbstractTree.VariableDeclaration, ILVariableScope*)
        v  (* name and type *)

    member x.BuildClass (program : AbstractTree.Program) =
        let variableDeclarations =    (* program = Declaration li*)
            program
            |> List.choose (fun x ->  (* find which one meets the requirements in choose function*)
                match x with
                | AbstractTree.StaticVariableDeclaration(x) -> Some(x)
                | _ -> None)
    
        let functionDeclarations =
            program
            |> List.choose (fun x ->
                match x with
                | AbstractTree.FunctionDeclaration(_, _, _, _ as a) -> Some a
                | _ -> None)

        let processFunctionDeclaration functionDeclaration =
            let ilMethodBuilder = new ILMethodBuilder(semanticAnalysisResult, variableMappings)
            ilMethodBuilder.BuildMethod functionDeclaration

        let builtInMethods = [       (* contains read and print operation*)
            {
                Name = "iread";
                ReturnType = typeof<int>;
                Parameters = [];
                Locals = [];
                Body = [ CallClr(typeof<System.Console>.GetMethod("ReadLine"))
                         CallClr(typeof<System.Convert>.GetMethod("ToInt32", [| typeof<string> |]))
                         Ret ];
            };
            {
                Name = "fread";
                ReturnType = typeof<float>;
                Parameters = [];
                Locals = [];
                Body = [ CallClr(typeof<System.Console>.GetMethod("ReadLine"))
                         CallClr(typeof<System.Convert>.GetMethod("ToDouble", [| typeof<string> |]))
                         Ret ];
            };
            {
                Name = "iprint";
                ReturnType = typeof<System.Void>;
                Parameters = [ { Type = typeof<int>; Name = "value"; }];
                Locals = [];
                Body = [ Ldarg(0s)
                         CallClr(typeof<System.Console>.GetMethod("WriteLine", [| typeof<int> |]))
                         Ret ];
            };
            {
                Name = "fprint";
                ReturnType = typeof<System.Void>;
                Parameters = [ { Type = typeof<float>; Name = "value"; }];
                Locals = [];
                Body = [ Ldarg(0s)    (* need int16 *)
                         CallClr(typeof<System.Console>.GetMethod("WriteLine", [| typeof<float> |]))
                         Ret ];
            } ]

        {
            Fields  = variableDeclarations |> List.map processStaticVariableDeclaration;   (* each declaration map into this function *)
            Methods = List.concat [ builtInMethods
                                    functionDeclarations |> List.map processFunctionDeclaration ];
        }