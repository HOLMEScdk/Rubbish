fslex --unicode RBCLex.fsl
fsyacc --module RBCPar RBCPar.fsy
fsi -r FsLexYacc.Runtime.dll AbstractTree.fs RBCPar.fs RBCLex.fs CIL.fs CatchErrors.fs GrammerAnalysis.fs CILturnUtili.fs  MakeCIL.fs CILBuild.fs Codegen.fs Compiler.fs

open RBC.Compiler
open System
open System.IO
open System.Reflection
open System.Reflection.Emit;;

% two tables symbolTable and functionTable classes %

let sourceFile = "test7.rbc";;
let code = File.ReadAllText(Path.Combine("test", sourceFile));;
let targetFileName = Path.Combine("test", Path.GetFileNameWithoutExtension(sourceFile) + ".exe");;
compileToFile targetFileName code;;
