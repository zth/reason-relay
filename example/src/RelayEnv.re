exception Graphql_error(string);

let fetchQuery: ReasonRelay.Network.fetchFunctionPromise =
  (operation, variables, _cacheConfig) =>
    Fetch.(
      fetchWithInit(
        "http://localhost:4000",
        Fetch.RequestInit.make(
          ~method_=Post,
          ~body=
            Js.Dict.fromList([
              ("query", Js.Json.string(operation##text)),
              ("variables", variables),
            ])
            |> Js.Json.object_
            |> Js.Json.stringify
            |> Fetch.BodyInit.make,
          ~headers=
            Fetch.HeadersInit.make({
              "content-type": "application/json",
              "accept": "application/json",
            }),
          (),
        ),
      )
      |> Js.Promise.then_(resp =>
           if (Response.ok(resp)) {
             Response.json(resp);
           } else {
             Js.Promise.reject(
               Graphql_error(
                 "Request failed: " ++ Response.statusText(resp),
               ),
             );
           }
         )
    );

let network =
  ReasonRelay.Network.makePromiseBased(~fetchFunction=fetchQuery, ());

let makeEnvironment = () =>
  ReasonRelay.Environment.make(
    ~network,
    ~store=ReasonRelay.Store.make(ReasonRelay.RecordSource.make()),
    (),
  );

let environment = makeEnvironment();
