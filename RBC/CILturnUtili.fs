module RBC.CILturnUtili
open System.Collections.Generic
open RBC.CIL
type CILVariableScope =
    | FieldScope of ILVariable
    | ArgumentScope of int16
    | LocalScope of int16

type VariableMappingDictionary = Dictionary<AbstractTree.VariableDeclaration, CILVariableScope>
(* define ILBuilder's type as follows some example*)
let typeOf =
    function
    | AbstractTree.Void  -> typeof<System.Void>
    | AbstractTree.Int   -> typeof<int>
    | AbstractTree.Float -> typeof<float>
    | AbstractTree.Bool  -> typeof<bool>
    | AbstractTree.Char  -> typeof<char>
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