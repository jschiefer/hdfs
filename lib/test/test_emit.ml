#light

open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Reflection
open System.IO

open Microsoft
open CSharp
open VisualBasic
open FSharp
open Quotations
open Typed
open Raw

(* Add a field to a class *)
let add_field attr (clas : CodeTypeDeclaration) (typ : string) (name : string) = 
  let fld = new CodeMemberField(typ, name)
  fld.Attributes <- attr
  clas.Members.Add(fld)

let add_private_field = add_field MemberAttributes.Private
let add_public_field = add_field MemberAttributes.Public

(* invoke a single method with given expression *)
let invoke_method cls mthd (expr : CodeExpression[]) = 
  let invokeExpression = new CodeMethodInvokeExpression(new CodeTypeReferenceExpression("Console"), 
                                                        "Write", 
                                                        expr );
  let expressionStatement = new CodeExpressionStatement( invokeExpression );    
  expressionStatement
  
///////////////////////////////////////////////////////////////


let s = File.Open("temp.cs", FileMode.Create);
let sw = new StreamWriter(s);

(* code provider *)
let codeProvider = new CSharpCodeProvider()
let copts = new CodeGeneratorOptions()

(* container for the code *)
let cunit = new CodeCompileUnit()

(* namespace *)
let cnamesp = new CodeNamespace("Simulator")
cunit.Namespaces.Add(cnamesp)

(* referenced namespaces *)
let using = new CodeNamespaceImport("System")
cnamesp.Imports.Add(using)

(* Create Class Declaration *)
let csim = new CodeTypeDeclaration()
csim.IsClass <- true
csim.Name <- "MySimulator"
csim.TypeAttributes <- TypeAttributes.Public

add_private_field csim "uint32" "temp32"
add_private_field csim "uint64" "temp64"

(* add class to namespace *)
cnamesp.Types.Add(csim)

(* generate code *)
codeProvider.GenerateCodeFromCompileUnit(cunit, sw, copts)

sw.Close()
s.Close()

