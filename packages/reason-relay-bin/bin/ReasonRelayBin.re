open Lib;

module GenerateFromFlow = {
  [@deriving yojson]
  type operation_type = {
    operation: string,
    [@default None]
    operation_value: option(string),
    [@default None]
    fragment_value: option((string, bool)),
  };

  [@deriving yojson]
  type config = {
    content: string,
    operation_type,
    operation_node: string,
    operation_hash: option(string),
    operation_request_id: option(string),
    raw_js: string,
  };
};

let () = {
  switch (Sys.argv[1]) {
  | exception (Invalid_argument(_)) =>
    print_endline("Invalid command.");
    exit(1);
  | "generate-from-flow" =>
    switch (
      Shexp_process.(eval(read_all))
      |> Yojson.Safe.from_string
      |> GenerateFromFlow.config_of_yojson
    ) {
    | Error(_) =>
      print_endline("Error parsing JSON");
      exit(1);
    | Ok(config) =>
      TypesTransformer.printFromFlowTypes(
        ~content=config.content,
        ~operationType=
          switch (config.operation_type) {
          | {operation: "query", operation_value: Some(queryName), _} =>
            Query(queryName)
          | {operation: "mutation", operation_value: Some(mutationName), _} =>
            Mutation(mutationName)
          | {
              operation: "subscription",
              operation_value: Some(subscriptionName),
              _,
            } =>
            Subscription(subscriptionName)
          | {
              operation: "fragment",
              fragment_value: Some((fragmentName, plural)),
              _,
            } =>
            Fragment(fragmentName, plural)
          | _ =>
            print_endline("No valid operation found");
            exit(1);
          },
        ~config={
          operation_node: config.operation_node,
          operation_hash: config.operation_hash,
          operation_request_id: config.operation_request_id,
          raw_js: config.raw_js,
        },
      )
      |> print_endline
    }
  | _ => ()
  };
};
