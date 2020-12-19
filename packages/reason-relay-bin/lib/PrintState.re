exception No_extractable_operations_found;

/**
 * Print the full state, and whatever utils/types etc are needed.
 */
let getPrintedFullState =
    (~operationType, ~config: Types.printConfig, state: Types.fullState)
    : string => {
  let finalStr = ref("/* @generated */\n\n");
  let addToStr = Utils.makeAddToStr(finalStr);

  let addSpacing = () => addToStr("\n\n\n");

  let definitions: ref(list(Types.rootType)) = ref([]);
  let addDefinition = Utils.makeAddToList(definitions);

  let typeDeclarations: ref(list(Types.rootType)) = ref([]);
  let addTypeDeclaration = Utils.makeAddToList(typeDeclarations);

  // Gather all type declarations and definitions.
  state.objects
  |> Tablecloth.List.sortBy(
       ~f=
         fun
         | {Types.originalFlowTypeName: Some(_)} => (-1)
         | {originalFlowTypeName: None} => 1,
     )
  |> Tablecloth.List.iter(
       ~f=
         fun
         | {Types.foundInUnion: false, recordName: Some(name)} as obj =>
           addTypeDeclaration(
             Types.(
               ObjectTypeDeclaration({
                 name,
                 definition: obj.definition,
                 atPath: obj.atPath,
               })
             ),
           )
         | _ => (),
     );

  // We check and add all definitions we've found to a list that'll later be printed as types.
  switch (state.variables) {
  | Some(variables) => addDefinition(Types.(Variables(Object(variables))))
  | None => ()
  };

  // Adds refetchVariables only to query output
  switch (state.variables, operationType) {
  | (Some(variables), Types.Query(_)) =>
    addDefinition(Types.(RefetchVariables(variables)))
  | _ => ()
  };

  switch (state.fragment, state.rawResponse) {
  | (None, Some(rawResponse)) =>
    addDefinition(Types.(RawResponse(Some(Object(rawResponse)))))
  | (None, None) => addDefinition(Types.(RawResponse(None)))
  | (Some(_), _) => ()
  };

  switch (state.response) {
  | Some(response) => addDefinition(Types.(Operation(Object(response))))
  | None => ()
  };

  switch (state.fragment) {
  | Some({definition, plural}) =>
    addDefinition(
      plural ? Types.PluralFragment(definition) : Types.Fragment(definition),
    )
  | None => ()
  };

  addSpacing();

  // Print definitions and declarations
  addToStr("module Types = {\n");

  // Print enums
  state.enums
  |> List.iter(enum => {
       addToStr(Printer.printEnum(enum));
       addSpacing();
     });

  // We turn off warning 30 because it's quite likely that record field labels will overlap in GraphQL
  addToStr("[@ocaml.warning \"-30\"];\n");

  let shouldIgnoreFragmentRefs =
    switch (operationType) {
    | Mutation(_) => true
    | _ => false
    };

  state.unions
  |> List.iter(({union, printName}: Types.unionInState) => {
       Printer.printUnionTypes(union, ~state, ~printName) |> addToStr
     });

  // Split declarations so we can print object declarations first and as mutuals
  let (otherDeclarations, objectDeclarations) =
    typeDeclarations^
    |> Tablecloth.List.splitWhen(
         ~f=
           fun
           | Types.ObjectTypeDeclaration(_) => true
           | _ => false,
       );

  let numObjectDeclarations = Tablecloth.List.length(objectDeclarations);

  // Print object declarations first

  Printer.(
    objectDeclarations
    |> List.iteri((index, def) => {
         def
         |> printRootType(
              ~recursiveMode=
                switch (index, numObjectDeclarations) {
                | (0, _) => Some(`Head)
                | (index, num) when index + 1 == num => Some(`Tail)
                | _ => Some(`Member)
                },
              ~state,
              ~ignoreFragmentRefs=shouldIgnoreFragmentRefs,
            )
         |> addToStr
       })
  );

  Printer.(
    otherDeclarations
    |> List.iter(def =>
         def
         |> printRootType(
              ~recursiveMode=None,
              ~state,
              ~ignoreFragmentRefs=shouldIgnoreFragmentRefs,
            )
         |> addToStr
       )
  );

  addSpacing();

  Printer.(
    definitions^
    |> List.iter(def => {
         def
         |> printRootType(
              ~state,
              ~ignoreFragmentRefs=shouldIgnoreFragmentRefs,
            )
         |> addToStr
       })
  );

  addSpacing();

  addToStr("};");
  addSpacing();

  if (state.unions |> List.length > 0) {
    state.unions
    |> List.iter(({union}: Types.unionInState) =>
         union |> Printer.printUnionConverters |> addToStr
       );
  };

  // This emits extra assets for the generated modules,
  // like code for converting nullable fields, enums and unions,
  // and code for extracting fragment refs.
  addSpacing();
  addToStr("module Internal = {");

  let rootObjects =
    state.objects
    |> Tablecloth.List.filter(
         ~f=
           fun
           | {Types.originalFlowTypeName: Some(_), recordName: Some(_)} =>
             true
           | _ => false,
       );

  switch (state.fragment) {
  | Some({definition}) =>
    addToStr(
      TypesTransformerUtils.printConverterAssets(
        ~rootObjects,
        ~definition,
        "fragment",
      ),
    );
    addSpacing();
  | None => ()
  };

  switch (state.response) {
  | Some(definition) =>
    switch (operationType) {
    | Query(_)
    | Mutation(_) =>
      addToStr(
        TypesTransformerUtils.printConverterAssets(
          ~rootObjects,
          ~direction=Wrap,
          ~nullableType=Null,
          ~definition=Object(definition),
          "wrapResponse",
        ),
      );
      addSpacing();
    | _ => ()
    };

    addToStr(
      TypesTransformerUtils.printConverterAssets(
        ~rootObjects,
        ~definition=Object(definition),
        "response",
      ),
    );
    addSpacing();
  | None => ()
  };

  switch (state.response, state.rawResponse) {
  | (Some(_), Some(definition)) =>
    switch (operationType) {
    | Query(_)
    | Mutation(_) =>
      addToStr(
        TypesTransformerUtils.printConverterAssets(
          ~rootObjects,
          ~direction=Wrap,
          ~nullableType=Null,
          ~definition=Object(definition),
          "wrapRawResponse",
        ),
      );
      addSpacing();
    | _ => ()
    };

    addToStr(
      TypesTransformerUtils.printConverterAssets(
        ~rootObjects,
        ~definition=Object(definition),
        "rawResponse",
      ),
    );
    addSpacing();
  | (Some(_), None) =>
    switch (operationType) {
    | Query(_)
    | Mutation(_) =>
      addToStr(
        "type wrapRawResponseRaw = wrapResponseRaw;"
        ++ "let convertWrapRawResponse = convertWrapResponse;",
      );
      addSpacing();
    | _ => ()
    };

    addToStr(
      "type rawResponseRaw = responseRaw;"
      ++ "let convertRawResponse = convertResponse;",
    );
    addSpacing();
  | (None, _) => ()
  };

  switch (state.variables) {
  | Some(definition) =>
    addToStr(
      TypesTransformerUtils.printConverterAssets(
        ~rootObjects,
        ~includeRaw=false,
        ~direction=Wrap,
        ~definition=Object(definition),
        "variables",
      ),
    );
    addSpacing();
  | None => ()
  };
  addToStr("};");
  addSpacing();

  // Print fragment assets
  switch (state.fragment) {
  | Some({name, plural}) =>
    addToStr(Printer.fragmentRefAssets(~plural, name));
    addSpacing();
  | None => ()
  };

  // Print query assets
  switch (operationType) {
  | Query(_) =>
    addToStr("type queryRef;");
    addSpacing();
  | _ => ()
  };

  // Utils that'll be included and accessible at the top level of the generated module
  addToStr("module Utils = {");
  let utilsContent = ref("");
  let addToUtils = str => utilsContent := utilsContent^ ++ str;
  let addSpacingToUtils = () => addToUtils("\n\n\n");

  // Enum toString functions
  state.enums
  |> List.iter(enum => enum |> Printer.printEnumToStringFn |> addToStr);

  // We print maker functions for all input objects
  state.objects
  |> Tablecloth.List.iter(
       ~f=
         fun
         | {Types.originalFlowTypeName: Some(typeName), definition} => {
             definition
             |> Printer.printObjectMaker(
                  ~targetType=Tablecloth.String.uncapitalize(typeName),
                  ~name="make_" ++ Tablecloth.String.uncapitalize(typeName),
                )
             |> addToUtils;
             addSpacingToUtils();
           }
         | _ => (),
     );

  // Add a maker function for the variables if variables exist
  switch (state.variables) {
  | None => ()
  | Some(variables) =>
    variables |> Printer.objHasPrintableContents
      ? {
        Printer.printObjectMaker(
          variables,
          ~targetType="variables",
          ~name="makeVariables",
        )
        |> addToUtils;
        addSpacingToUtils();
      }
      : ()
  };

  // Emit make function for optimistic responses
  switch (operationType, state.response) {
  | (Mutation(_), Some(response)) =>
    state.objects
    |> Tablecloth.List.filter_map(
         ~f=
           fun
           | {
               Types.definition,
               originalFlowTypeName: None,
               recordName: Some(name),
             } =>
             Some((name, definition))
           | _ => None,
       )
    |> List.iter(((name, obj)) => {
         Printer.printObjectMaker(
           obj,
           ~targetType=name,
           ~name="make_" ++ name,
         )
         |> addToUtils;
         addSpacingToUtils();
       });

    Printer.printObjectMaker(
      response,
      ~targetType="rawResponse",
      ~name="makeOptimisticResponse",
    )
    |> addToUtils;
    addSpacingToUtils();
  | _ => ()
  };

  // Open Types locally here if we have any content to print
  if (utilsContent^ != "") {
    addToStr("open Types;");
  };

  addToStr(utilsContent^ ++ "};");
  addSpacing();

  // This adds operationType, which is referenced in the raw output of the Relay
  // runtime representation.
  addToStr(Printer.operationType(operationType));
  addSpacing();

  switch (state) {
  | {fragment: None, response: None, variables: None} =>
    raise(No_extractable_operations_found)
  | _ => ()
  };

  finalStr^;
};
