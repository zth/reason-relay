open Lib;

module GenerateFromFlow = {
  [@deriving yojson]
  type connection_info = {
    key: string,
    at_object_path: list(string),
    field_name: string,
  };

  [@deriving yojson]
  type print_config = {
    [@default None]
    variables_holding_connection_ids: option(list(string)),
    [@default None]
    connection: option(connection_info),
  };

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
    print_config,
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
          variables_holding_connection_ids:
            config.print_config.variables_holding_connection_ids,
          connection:
            switch (config.print_config.connection) {
            | None => None
            | Some(conn) =>
              Some({
                key: conn.key,
                atObjectPath: List.rev(conn.at_object_path),
                fieldName: conn.field_name,
              })
            },
        },
      )
      |> print_endline
    }
  | _ => ()
  };
};
