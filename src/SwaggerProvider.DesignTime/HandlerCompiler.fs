namespace SwaggerProvider.Internal.Compilers

open ProviderImplementation.ProvidedTypes
open FSharp.Data.Runtime.NameUtils
open SwaggerProvider.Internal
open SwaggerProvider.Internal.Schema

open System
open System.Collections.Generic
open System.Threading.Tasks
open FSharp.Data

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.ExprShape
open System.Text.RegularExpressions

/// Object for compiling operations.
type HandlerCompiler (schema:SwaggerObject, defCompiler:DefinitionCompiler) =
    let compileHandler (methodName:string) (op:OperationObject) =
        if String.IsNullOrWhiteSpace methodName
            then failwithf "Operation name could not be empty. See '%s/%A'" op.Path op.Type

        // TODO: provide op-specific signature using specified parameters
        let handlerTy = typeof<Func<IDictionary<string, obj>, Task>>
        let parameters =
            (*
                TODO: create parameterized OWIN handler using specified parameters
                TODO: generate handler function type that returns expected result type
            // Generate handler function signature,
            // e.g. (fun (p1, p2, ..., pn) -> resultType)
            [let required, optional = op.Parameters |> Array.partition (fun x->x.Required)
             for x in Array.append required optional do
                let paramName = niceCamelName x.Name
                yield ProvidedParameter(paramName, defCompiler.CompileTy methodName paramName x.Type x.Required)
             // Provide the raw OWIN dictionary in case it's required for additional lookups.
             yield ProvidedParameter("env", typeof<Option<IDictionary<string, obj>>>)
            ]
            *)
            [ProvidedParameter("handler", handlerTy)]
        
        let m = ProvidedMethod(methodName, parameters, typeof<unit>)
        if not <| String.IsNullOrEmpty(op.Summary)
            then m.AddXmlDoc(op.Summary) // TODO: Use description of parameters in docs
        if op.Deprecated
            then m.AddObsoleteAttribute("Operation is deprecated", false)

        // Whenver the method is invoked, it should register/update a route with the specified handler.
        m.InvokeCode <- fun args ->
            let thisTy = typeof<SwaggerProvider.Internal.ProvidedSwaggerApiBaseType>
            let this = Expr.Coerce(args.[0], thisTy)
            let httpMethod = Expr.Value (op.Type.ToString().ToUpper())
            let path = Expr.Value op.Path
            // TODO: augment handler to accept OWIN environment variable.
            // TODO: retrieve expected parameters from OWIN environment and pass to parameterized handler.
            // TODO: write type result of handler function to the "owin.ResponseBody" stream and set appropriate headers.
            let handler = Expr.Coerce(args.[1], handlerTy)
            Expr.Call(this, thisTy.GetMethod("AddRoute", Reflection.BindingFlags.Instance ||| Reflection.BindingFlags.Public), [httpMethod; path; handler])
        
        m

    /// Compiles the operation.
    member __.CompilePaths(ignoreOperationId) =
        let methodNameScope = UniqueNameGenerator()
        let pathToName opType (opPath:String) =
            String.Join("_",
                [|
                    yield opType.ToString()
                    yield!
                        opPath.Split('/')
                        |> Array.filter (fun x ->
                            not <| (String.IsNullOrEmpty(x) || x.StartsWith("{")))
                |])
        let getMethodNameCandidate (op:OperationObject) =
            if ignoreOperationId || String.IsNullOrWhiteSpace(op.OperationId)
            then pathToName op.Type op.Path
            else op.OperationId

        List.ofArray schema.Paths
        |> List.map (fun op ->
            let methodNameCandidate = nicePascalName <| getMethodNameCandidate op
            compileHandler (methodNameScope.MakeUnique methodNameCandidate) op)
