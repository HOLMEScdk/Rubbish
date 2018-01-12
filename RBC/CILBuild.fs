module RBC.CILBuild
open System.Collections.Generic
open RBC.GrammerAnalysis
open RBC.CIL
open RBC.CILturnUtili
open RBC.MakeCIL
type CILBuild(GrammerAnalysisResult) =
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
            let ilMethodBuilder = new CILBuildMethod(GrammerAnalysisResult, variableMappings)
            ilMethodBuilder.BuildMethod functionDeclaration

        let builtInMethods = [       (* contains read and print operation*)
            {
                Name = "readInt";
                ReturnType = typeof<int>;
                Parameters = [];
                Locals = [];
                Body = [ CallClr(typeof<System.Console>.GetMethod("ReadLine"))
                         CallClr(typeof<System.Convert>.GetMethod("ToInt32", [| typeof<string> |]))
                         Ret ];
            };
            {
                Name = "readFloat";
                ReturnType = typeof<float>;
                Parameters = [];
                Locals = [];
                Body = [ CallClr(typeof<System.Console>.GetMethod("ReadLine"))
                         CallClr(typeof<System.Convert>.GetMethod("ToDouble", [| typeof<string> |]))
                         Ret ];
            };
            {
                Name = "printI";
                ReturnType = typeof<System.Void>;
                Parameters = [ { Type = typeof<int>; Name = "value"; }];
                Locals = [];
                Body = [ Ldarg(0s)
                         CallClr(typeof<System.Console>.GetMethod("WriteLine", [| typeof<int> |]))
                         Ret ];
            };
            {
                Name = "printF";
                ReturnType = typeof<System.Void>;
                Parameters = [ { Type = typeof<float>; Name = "value"; }];
                Locals = [];
                Body = [ Ldarg(0s)    (* need int16 *)
                         CallClr(typeof<System.Console>.GetMethod("WriteLine", [| typeof<float> |]))
                         Ret ];
            }
            {
                Name = "printCh";
                ReturnType = typeof<System.Void>;
                Parameters = [ { Type = typeof<char>; Name = "value"; }];
                Locals = [];
                Body = [ Ldarg(0s)    (* need int16 *)
                         CallClr(typeof<System.Console>.GetMethod("WriteLine", [| typeof<char> |]))
                         Ret ];
            }]
        {         (*record*)
            Fields  = variableDeclarations |> List.map processStaticVariableDeclaration;   (* each declaration map into this function *)
            Methods = List.concat [ builtInMethods
                                    functionDeclarations |> List.map processFunctionDeclaration ];
        }