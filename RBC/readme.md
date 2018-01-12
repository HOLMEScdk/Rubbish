fsi -r Piglet.dll AbstractTree.fs CIL.fs CompilerException.fs ParsingUtilities.fs Parser.fs GrammerAnalysis.fs CILturnUtili.fs MakeCIL.fs Codegen.fs Compiler.fs 

fslex --unicode RBCLex.fsl
fsyacc --module RBCPar RBCPar.fsy
fsi -r FsLexYacc.Runtime.dll AbstractTree.fs RBCPar.fs RBCLex.fs CIL.fs CatchErrors.fs GrammerAnalysis.fs CILturnUtili.fs  MakeCIL.fs CILBuild.fs Codegen.fs Compiler.fs



open RBC.Compiler
open System
open System.IO
open System.Reflection
open System.Reflection.Emit;;


let sourceFile = "test6.minic";;
let code = File.ReadAllText(Path.Combine("Sources", sourceFile));;
let targetFileName = Path.Combine("Sources", Path.GetFileNameWithoutExtension(sourceFile) + ".exe");;
compileToFile targetFileName code;;


% two tables symbolTable and functionTable classes %

