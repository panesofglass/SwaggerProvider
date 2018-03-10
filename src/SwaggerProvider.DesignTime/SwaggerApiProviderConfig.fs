namespace SwaggerProvider

open System.Reflection
open ProviderImplementation.ProvidedTypes
open System
open System.Net.Http
open Swagger.Parser
open SwaggerProvider.Internal.Compilers

module private SwaggerApiProviderConfig =
    let NameSpace = "SwaggerProvider"

    let internal typedSwaggerApiProvider (ctx: ProvidedTypesContext) asmLocation =
        let asm = Assembly.LoadFrom asmLocation
        let swaggerApiProvider = ProvidedTypeDefinition(asm, NameSpace, "SwaggerApiProvider", Some typeof<obj>, isErased = false)

        let staticParam name ty doc (def: 'a option) =
            let p =
                match def with
                | Some d -> ProvidedStaticParameter(name, ty, d)
                | None -> ProvidedStaticParameter(name, ty)
            p.AddXmlDoc(doc)
            p

        let staticParams =
            [ ProvidedStaticParameter("Schema", typeof<string>)
              ProvidedStaticParameter("Headers", typeof<string>, "")
              ProvidedStaticParameter("IgnoreOperationId", typeof<bool>, false)
              ProvidedStaticParameter("IgnoreControllerPrefix", typeof<bool>, true)
              ProvidedStaticParameter("ProvideNullable", typeof<bool>, false)
              ProvidedStaticParameter("PreferAsync", typeof<bool>, false)]

        //TODO: Add use operationID flag
        swaggerApiProvider.AddXmlDoc
            """<summary>Statically typed Swagger API provider.</summary>
               <param name='Schema'>Url or Path to Swagger schema file.</param>
               <param name='Headers'>Headers that will be used to access the schema.</param>
               <param name='IgnoreOperationId'>IgnoreOperationId tells SwaggerProvider not to use `operationsId` and generate method names using `path` only. Default value `false`</param>
               <param name='IgnoreControllerPrefix'>IgnoreControllerPrefix tells SwaggerProvider not to parse `operationsId` as `<controllerName>_<methodName>` and generate one client class for all operations. Default value `true`</param>
               <param name='ProvideNullable'>Provide `Nullable<_>` for not required properties, instread of `Option<_>`</param>
               <param name='PreferAsync'>PreferAsync tells the SwaggerProvider to generate async actions of type `Async<'T>` instead of `Task<'T>`. Defaults to `false`</param>"""

        swaggerApiProvider.DefineStaticParameters(
            parameters=staticParams,
            instantiationFunction = (fun typeName args ->
                let tempAsm = ProvidedAssembly()
                let schemaPathRaw = args.[0] :?> string
                let ignoreOperationId = args.[2] :?> bool
                let ignoreControllerPrefix = args.[3] :?> bool
                let provideNullable = args.[4] :?> bool
                let asAsync = args.[5] :?> bool

                let schemaData =
                    match schemaPathRaw.StartsWith("http", true, null) with
                    | true  ->
                        let headersStr = args.[1] :?> string
                        let headers =
                            headersStr.Split('|')
                            |> Seq.choose (fun x ->
                                let pair = x.Split('=')
                                if (pair.Length = 2)
                                then Some (pair.[0],pair.[1])
                                else None
                            )
                        let request = new HttpRequestMessage(HttpMethod.Get, schemaPathRaw)
                        for (name, value) in headers do 
                            request.Headers.Add(name, value)
                        // using a custom handler means that we can set the default credentials.
                        let handler = new HttpClientHandler(UseDefaultCredentials = true)
                        let client = new HttpClient(handler)
                        async {
                            let! response = client.SendAsync(request) |> Async.AwaitTask
                            return! response.Content.ReadAsStringAsync() |> Async.AwaitTask
                        } |> Async.RunSynchronously
                    | false ->
                        schemaPathRaw |> IO.File.ReadAllText

                let schema = SwaggerParser.parseSchema schemaData

                // Create Swagger provider type
                let baseTy = Some typeof<obj>
                let ty = ProvidedTypeDefinition(tempAsm, NameSpace, typeName, baseTy, isErased = false)
                ty.AddXmlDoc ("Swagger API Provider for " + schema.Host)
                ty.AddMember <| ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>)

                let defCompiler = DefinitionCompiler(schema, provideNullable)
                let opCompiler = HandlerCompiler(schema, defCompiler, ignoreControllerPrefix, ignoreOperationId, asAsync)

                opCompiler.CompileProvidedHandlers(defCompiler.Namespace)
                ty.AddMembers <| defCompiler.Namespace.GetProvidedTypes() // Add all provided types

                tempAsm.AddTypes [ty]

                ty
            ))
        swaggerApiProvider


