module RBC.CIL

open System
(* define intermediate presenation *)
(*fields and methods start*)
(* for each function in the program,generate an MSIL method. 
For each global variable declaration, generate a field.*)

type ILClass = {
        Fields  : ILVariable list;
        Methods : ILMethod list;
    }
(*the method's content*)
and ILVariable = {(*System.Type*)
        Type  : Type;
        Name  : string;
    }


and ILMethod = {
        Name       : string;
        ReturnType : Type;
        Parameters : ILVariable list;
        Locals     : ILVariable list;
        Body       : ILOpCode list;
    }

and ILLabel = int

and ILOpCode =
    | Add
    | Br of ILLabel    (* Br needs to know the ILLabel to break *)
    | Brfalse of ILLabel
    | Brtrue of ILLabel
    | Call of string
    | CallClr of System.Reflection.MethodInfo
    | Ceq
    | Cge
    | Cgt
    | Cle
    | Clt
    | Dup
    | Div
    | Label of ILLabel
    | Ldarg of int16
    | Ldc_I4 of int
    | Ldc_R8 of float
    | Ldelem of Type
    | Ldlen
    | Ldloc of int16
    | Ldsfld of ILVariable
    | Mul
    | Neg
    | Newarr of Type
    | Pop
    | Rem
    | Ret
    | Starg of int16
    | Stelem of Type
    | Stloc of int16
    | Stsfld of ILVariable
    | Sub
