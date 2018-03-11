﻿module Swagger.PetStore.Tests

open SwaggerProvider
open Expecto
open System

type PetStore = SwaggerProvider<"http://petstore.swagger.io/v2/swagger.json", PreferAsync = true>
type PetStoreNullable = SwaggerProvider<"http://petstore.swagger.io/v2/swagger.json", ProvideNullable = true>
let store = PetStore.Client()
let apiKey = "test-key"

[<Tests>]
let petStoreTests =
  testList "All/TP PetStore Tests" [

    testCase "Test provided Host property" <| fun _ ->
        Expect.equal store.Host "http://petstore.swagger.io" "value from schema"
        store.Host <- "https://petstore.swagger.io"
        Expect.equal store.Host "https://petstore.swagger.io" "Modified value"
        store.Host <- "http://petstore.swagger.io"
        Expect.equal store.Host "http://petstore.swagger.io" "original value"

    testCase "instantiate provided objects" <| fun _ ->
        let pet = PetStore.Pet(Name = "foo")
        Expect.equal pet.Name "foo" "access initial value"
        Expect.stringContains (pet.ToString()) "foo" "ToString"
        pet.Name <- "bar"
        Expect.equal pet.Name "bar" "access modified value"
        Expect.stringContains (pet.ToString()) "bar" "ToString"

    testCaseAsync "call provided methods" <| async {
        try
            do! store.DeletePet(1337L, Some apiKey)
        with
        | _ -> ()

        let tag = PetStore.Tag(None, Some "foobar")
        Expect.stringContains (tag.ToString()) "foobar" "ToString"
        let pet = PetStore.Pet("foo", [||], Some 1337L)
        Expect.stringContains (pet.ToString()) "1337" "ToString"

        try
            do! store.AddPet(pet)
        with
        | exn ->
            let msg = if exn.InnerException = null then exn.Message
                      else exn.InnerException.Message
            failwithf "Adding pet failed with message: %s" msg

        let! pet2 = store.GetPetById(1337L)
        Expect.equal pet.Name     pet2.Name     "same Name"
        Expect.equal pet.Id       pet.Id        "same Id"
        Expect.equal pet.Category pet2.Category "same Category"
        Expect.equal pet.Status   pet2.Status   "same Status"
        Expect.notEqual pet pet2 "different objects"
    }

    testCase "create types with Nullable properties" <| fun _ ->
        let tag = PetStoreNullable.Tag(Nullable<_>(), "foobar")
        Expect.stringContains (tag.ToString()) "foobar" "ToString"
        let tag2 = PetStoreNullable.Tag (Name = "foobar")
        Expect.stringContains (tag2.ToString()) "foobar" "ToString"

        let pet = PetStoreNullable.Pet("foo", [||], Nullable(1337L))
        Expect.stringContains (pet.ToString()) "1337" "ToString"
        let pet2 = PetStoreNullable.Pet (Name="foo", Id = Nullable(1337L))
        Expect.stringContains (pet2.ToString()) "1337" "ToString"
  ]

open System.Net
open System.Net.Http
open System.Threading.Tasks

type PetStoreServer = SwaggerApiProvider<"http://petstore.swagger.io/v2/swagger.json">
let server = PetStoreServer.Handler()

// Define AddPet handler
server.AddPet(fun req ->
    let response = new HttpResponseMessage(Net.HttpStatusCode.Created, RequestMessage = req)
    Task.FromResult(response)
)

let makeRequest (httpMethod:string) (path:string) (body:byte[] option) =
    let request = new HttpRequestMessage(HttpMethod(httpMethod), "http://petstore.swagger.io/v2" + path)
    match body with
    | Some bytes -> request.Content <- new ByteArrayContent(bytes)
    | None -> ()
    request

[<Tests>]
let petStoreServerTests =
  testList "All/TP PetStore Server Tests" [

    testCase "GET /pet/findByStatus handler responds with 404 Not Found" <| fun _ ->
        let req = makeRequest "GET" "/pet/findByStatus" None
        let result = server.Invoke(req).Result
        Expect.equal result.StatusCode HttpStatusCode.NotFound "not found"

    testCase "GET /pet handler responds with 405 Method Not Allowed" <| fun _ ->
        let req = makeRequest "GET" "/pet" None
        let result = server.Invoke(req).Result
        Expect.equal result.StatusCode HttpStatusCode.MethodNotAllowed "method not allowed"

    testCase "POST /pet handler responds with expected result" <| fun _ ->
        let req = makeRequest "POST" "/pet" None
        let result = server.Invoke(req).Result
        Expect.equal result.StatusCode HttpStatusCode.Created "created"
  ]