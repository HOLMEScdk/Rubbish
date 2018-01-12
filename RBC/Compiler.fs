module RBC.Compiler
open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Text
open RBC.AbstractTree
open RBC.CILBuild

let compile (assemblyBuilder : AssemblyBuilder) code =
    let format:Printf.TextWriterFormat<_> = "%s"
    let assemblyName = assemblyBuilder.GetName()
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name, assemblyName.Name + ".exe", true)

    (*let program = Parser.parse code*)
    let program = RBCPar.Main RBCLex.Token (Lexing.LexBuffer<char>.FromString(code))
    let temp = program.ToString()
    printfn format temp
    let GrammerAnalysisResult = GrammerAnalysis.analyze program
    let ilBuilder = new CILBuild(GrammerAnalysisResult)
    let ilBuilder_String = ilBuilder.ToString()
    printfn format ilBuilder_String
    printfn "%s" "--------------------------"
    let ilClass = ilBuilder.BuildClass program
    let ilClass_String = ilClass.ToString()
    printfn format ilClass_String
    let codeGenerator = new CodeGenerator(moduleBuilder, ilClass, assemblyName.Name)
    let (compiledType, entryPoint) = codeGenerator.GenerateType()
    assemblyBuilder.SetEntryPoint entryPoint
    (compiledType, entryPoint)

let compileToFile fileName code =
    let assemblyName = new AssemblyName (Path.GetFileNameWithoutExtension fileName)
    let assemblyBuilder =
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            assemblyName, AssemblyBuilderAccess.RunAndSave,
            Path.GetDirectoryName(fileName))
    let (_, _) = compile assemblyBuilder code
    assemblyBuilder.Save (Path.GetFileName fileName)