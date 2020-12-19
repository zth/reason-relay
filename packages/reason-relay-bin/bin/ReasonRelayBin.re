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
    operation_hash: string,
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
          | {operation: "Query", operation_value: Some(queryName), _} =>
            Query(queryName)
          | {operation: "Mutation", operation_value: Some(mutationName), _} =>
            Mutation(mutationName)
          | {
              operation: "Subscription",
              operation_value: Some(subscriptionName),
              _,
            } =>
            Subscription(subscriptionName)
          | {
              operation: "Fragment",
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
          raw_js: config.raw_js,
        },
      )
      |> print_endline
    }
  | _ => ()
  };
};
