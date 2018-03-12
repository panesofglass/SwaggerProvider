namespace SwaggerProvider.Internal.Compilers

open ProviderImplementation.ProvidedTypes
open FSharp.Data.Runtime.NameUtils
open Swagger.Parser.Schema
open Swagger.Internal

open System

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.ExprShape
open System.Text.RegularExpressions
open SwaggerProvider.Internal
open System.Net.Http
open System.Collections.Generic

/// Object for compiling operations.
type HandlerCompiler (schema:SwaggerObject, defCompiler:DefinitionCompiler, ignoreControllerPrefix, ignoreOperationId, asAsync: bool) =
    let compileHandler (methodName:string) (op:OperationObject) =
        if String.IsNullOrWhiteSpace methodName
            then failwithf "Operation name could not be empty. See '%s/%A'" op.Path op.Type

        let parameters =
            /// handles deduping Swagger parameter names if the same parameter name 
            /// appears in multiple locations in a given operation definition.
            let uniqueParamName existing (current: ParameterObject) = 
                let name = niceCamelName current.Name
                if Set.contains name existing 
                then
                    Set.add current.UnambiguousName existing, current.UnambiguousName
                else
                    Set.add name existing, name

            let required, optional = op.Parameters |> Array.partition (fun x->x.Required)

            Array.append required optional
            |> Array.fold (fun (names,parameters) current ->
               let (names, paramName) = uniqueParamName names current
               let paramType = defCompiler.CompileTy methodName paramName current.Type current.Required
               let providedParam =
                   if current.Required then ProvidedParameter(paramName, paramType)
                   else
                       let paramDefaultValue = defCompiler.GetDefaultValue paramType
                       ProvidedParameter(paramName, paramType, false, paramDefaultValue)
               (names, providedParam :: parameters)
            ) (Set.empty, [])
            |> snd
            // because we built up our list in reverse order with the fold, 
            // reverse it again so that all required properties come first
            |> List.rev

        // find the innner type value
        let retTy =
            let okResponse = // BUG : wrong selector
                op.Responses |> Array.tryFind (fun (code, _) ->
                    (code.IsSome && (code.Value = 200 || code.Value = 201)) || code.IsNone)
            match okResponse with
            | Some (_,resp) ->
                match resp.Schema with
                | None -> None
                | Some ty -> Some <| defCompiler.CompileTy methodName "Response" ty true
            | None -> None

        let overallReturnType =
            ProvidedTypeBuilder.MakeGenericType(
                (if asAsync 
                 then typedefof<Async<unit>> 
                 else typedefof<System.Threading.Tasks.Task<unit>>),
                [defaultArg retTy (typeof<HttpResponseMessage>)]
            )

        // TODO: provide op-specific signature using specified parameters
        let handlerTy =
            (*
            ProvidedTypeBuilder.MakeGenericType(
                typedefof<System.Func<unit, unit>>,
                // TODO: provide generated parameter(s)
                [typeof<HttpRequestMessage>; overallReturnType]
            )
            *)
            typeof<Func<HttpRequestMessage, Threading.Tasks.Task<HttpResponseMessage>>>
        let handlerParameter =
            [ProvidedParameter("handler", handlerTy)]

        let m = ProvidedMethod(methodName, handlerParameter, typeof<unit>, invokeCode = fun args ->
            let thisTy = typeof<ProvidedSwaggerApiBaseType>
            let this = Expr.Coerce(args.[0], thisTy) |> Expr.Cast<ProvidedSwaggerApiBaseType>
            let httpMethod = Expr.Value (op.Type.ToString().ToUpper())
            let path = Expr.Value op.Path
            // TODO: augment handler to accept HttpRequestMessage.
            // TODO: retrieve expected parameters from HttpRequestMessage and pass to parameterized handler.
            // TODO: map result of handler function to the returned HttpResponseMessage.
            let handler = Expr.Coerce(args.[1], handlerTy)
            Expr.Call(this, thisTy.GetMethod("AddRoute", Reflection.BindingFlags.Instance ||| Reflection.BindingFlags.Public), [httpMethod; path; handler])
        )

        if not <| String.IsNullOrEmpty(op.Summary)
            then m.AddXmlDoc(op.Summary) // TODO: Use description of parameters in docs
        if op.Deprecated
            then m.AddObsoleteAttribute("Operation is deprecated", false)
        m

    static member GetMethodNameCandidate (op:OperationObject) skipLength ignoreOperationId =
        if ignoreOperationId || String.IsNullOrWhiteSpace(op.OperationId)
        then
            [|  yield op.Type.ToString()
                yield!
                    op.Path.Split('/')
                    |> Array.filter (fun x ->
                        not <| (String.IsNullOrEmpty(x) || x.StartsWith("{")))
            |] |> fun arr -> String.Join("_", arr)
        else op.OperationId.Substring(skipLength)
        |> nicePascalName

    member __.CompileProvidedHandlers(ns:NamespaceAbstraction) =
        let defaultHost =
            let protocol =
                match schema.Schemes with
                | [||]  -> "http" // Should use the scheme used to access the Swagger definition itself.
                | array -> array.[0]
            sprintf "%s://%s" protocol schema.Host
        let baseTy = Some typeof<ProvidedSwaggerBaseType>
        let baseCtor = baseTy.Value.GetConstructors().[0]

        List.ofArray schema.Paths
        |> List.groupBy (fun x ->
            if ignoreControllerPrefix then String.Empty //
            else
                let ind = x.OperationId.IndexOf("_")
                if ind <= 0 then String.Empty
                else x.OperationId.Substring(0, ind) )
        |> List.iter (fun (handlerName, operations) ->
            let tyName = ns.ReserveUniqueName handlerName "Handler"
            let ty = ProvidedTypeDefinition(tyName, baseTy, isErased = false, hideObjectMethods = true)
            ns.RegisterType(tyName, ty)
            ty.AddXmlDoc (sprintf "Handler for '%s_*' operations" handlerName)

            ty.AddMember <|
                ProvidedConstructor(
                    [ProvidedParameter("host", typeof<string>, optionalValue = defaultHost)],
                    invokeCode = (fun args ->
                        match args with
                        | [] -> failwith "Generated constructors should always pass the instance as the first argument!"
                        | _ -> <@@ () @@>),
                    BaseConstructorCall = fun args -> (baseCtor, args))

            let methodNameScope = UniqueNameGenerator()
            operations |> List.map (fun op ->
                let skipLength = if String.IsNullOrEmpty handlerName then 0 else handlerName.Length + 1
                let name = HandlerCompiler.GetMethodNameCandidate op skipLength ignoreOperationId
                compileHandler (methodNameScope.MakeUnique name) op)
            |> ty.AddMembers
        )
