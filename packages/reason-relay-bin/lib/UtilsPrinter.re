open Types;

type instruction =
  | ConvertNullableProp
  | ConvertNullableArrayContents
  | ConvertUnion(string)
  | ConvertCustomField(string)
  | RootObject(string)
  | HasFragments
  | ConvertTopLevelNodeField(string);

type instructionContainer = {
  atPath: list(string),
  instruction,
};

type converterInstructions = Hashtbl.t(string, Hashtbl.t(string, string));

type allConverterInstructions = Hashtbl.t(string, converterInstructions);

type objectAssets = {
  converterInstructions: allConverterInstructions,
  convertersDefinition: string,
};

let printConvertersMap = (map: Hashtbl.t(string, string)): string =>
  if (Hashtbl.length(map) == 0) {
    "()";
  } else {
    let str = ref("{");
    let addToStr = s => str := str^ ++ s;

    map
    |> Hashtbl.iter((key, value) =>
         addToStr("\"" ++ key ++ "\": " ++ value ++ ",")
       );

    addToStr("};");
    str^;
  };

type conversionDirection =
  | Wrap
  | Unwrap;

let definitionToAssets =
    (
      ~rootObjects: list(finalizedObj),
      ~direction=Unwrap,
      definition: rootStructure,
    )
    : objectAssets => {
  let instructions: ref(list(instructionContainer)) = ref([]);
  let addInstruction = i =>
    instructions := List.concat([instructions^, [i]]);

  let inputObjectInstructions: Hashtbl.t(string, converterInstructions) =
    Hashtbl.create(10);

  let addInputObjInstruction = (name, instructions) =>
    Hashtbl.add(inputObjectInstructions, name, instructions);

  let converters = Hashtbl.create(10);

  let rec traversePropType =
          (
            ~converters,
            ~inRootObject,
            ~addInstruction,
            ~propName,
            ~currentPath,
            propType: propType,
          ) =>
    switch (propType) {
    | DataId
    | Scalar(_)
    | Enum(_)
    | StringLiteral(_)
    | FragmentRefValue(_) => ()
    | TypeReference(name) =>
      switch (
        rootObjects
        |> Tablecloth.List.find(
             ~f=
               fun
               | {recordName} when recordName == Some(name) => true
               | _ => false,
           ),
        Hashtbl.find_opt(inputObjectInstructions, name),
      ) {
      | (Some(obj), None) =>
        addInstruction({atPath: currentPath, instruction: RootObject(name)});

        if (inRootObject != Some(name)) {
          // Claim this instruction spot for the current input object. It'll be overwritten below with the real instructions.
          // If we don't do this, `makeRootObjectInstruction` below would put us in an endless loop if this input object has
          // circular dependencies on other input objects.
          addInputObjInstruction(name, Hashtbl.create(10));

          obj.definition
          |> makeRootObjectInstruction(~name, ~converters)
          |> addInputObjInstruction(name);
        };
      | (Some(_), Some(_)) =>
        addInstruction({atPath: currentPath, instruction: RootObject(name)})
      | (None, Some(_) | None) =>
        // If none of the above matches, this might be a custom field/custom module supplied by the user. We use a heuristic
        // (is this a module and not a regular type?) to determine.
        if (Utils.isModuleName(name)) {
          addInstruction({
            atPath: currentPath,
            instruction: ConvertCustomField(name),
          });

          Hashtbl.add(
            converters,
            name,
            switch (direction) {
            | Wrap => name ++ ".serialize"
            | Unwrap => name ++ ".parse"
            },
          );
        }
      }

    | Union({atPath, members} as union) =>
      Hashtbl.add(
        converters,
        Printer.makeUnionName(atPath),
        switch (direction) {
        | Wrap => Printer.printUnionWrapFnReference(union)
        | Unwrap => Printer.printUnionUnwrapFnReference(union)
        },
      );

      addInstruction({
        atPath: currentPath,
        instruction: ConvertUnion(Printer.makeUnionName(atPath)),
      });

      members
      |> Tablecloth.List.iter(~f=member => {
           member.shape.values
           |> traverseProps(
                ~converters,
                ~inRootObject,
                ~addInstruction,
                ~currentPath=[
                  member.name |> Tablecloth.String.toLower,
                  ...currentPath,
                ],
              )
         });
    | Array({nullable, propType}) =>
      if (nullable) {
        addInstruction({
          atPath: currentPath,
          instruction: ConvertNullableArrayContents,
        });
      };

      propType
      |> traversePropType(
           ~inRootObject,
           ~addInstruction,
           ~propName,
           ~currentPath,
           ~converters,
         );
    | TopLevelNodeField(name, {values}) =>
      addInstruction({
        atPath: currentPath,
        instruction: ConvertTopLevelNodeField(name),
      });

      values
      |> traverseProps(
           ~converters,
           ~inRootObject,
           ~addInstruction,
           ~currentPath,
         );
    | Object({values}) =>
      values
      |> traverseProps(
           ~converters,
           ~inRootObject,
           ~addInstruction,
           ~currentPath,
         )
    }
  and traverseProps =
      (
        ~currentPath,
        ~addInstruction,
        ~converters,
        ~inRootObject: option(string),
        propValues,
      ) => {
    let hasFragments = ref(false);

    propValues
    |> Tablecloth.List.iter(~f=p =>
         switch (p) {
         | FragmentRef(_) => hasFragments := true
         | Prop(name, {nullable, propType}) =>
           let newPath = [name, ...currentPath];

           if (nullable) {
             addInstruction({
               atPath: newPath,
               instruction: ConvertNullableProp,
             });
           };

           propType
           |> traversePropType(
                ~converters,
                ~inRootObject,
                ~addInstruction,
                ~currentPath=newPath,
                ~propName=name,
              );
         }
       );

    if (hasFragments^) {
      addInstruction({atPath: currentPath, instruction: HasFragments});
    };
  }
  and makeRootObjectInstruction = (~name, ~converters, obj: object_) => {
    let instructions: ref(list(instructionContainer)) = ref([]);
    let addInstruction = i =>
      instructions := List.concat([instructions^, [i]]);

    let () =
      obj.values
      |> traverseProps(
           ~converters,
           ~inRootObject=Some(name),
           ~addInstruction,
           ~currentPath=[],
         );

    makeConverterInstructions(instructions^);
  }
  and makeConverterInstructions = instructions => {
    let dict: converterInstructions = Hashtbl.create(10);

    instructions
    |> Tablecloth.List.iter(~f=instruction => {
         let key =
           instruction.atPath
           |> Tablecloth.List.reverse
           |> Tablecloth.String.join(~sep="_");

         let action =
           switch (instruction.instruction) {
           | ConvertNullableProp => ("n", "")
           | ConvertNullableArrayContents => ("na", "")
           | ConvertUnion(name) => ("u", name)
           | ConvertCustomField(name) => ("c", name)
           | RootObject(name) => ("r", name)
           | ConvertTopLevelNodeField(typeName) => ("tnf", typeName)
           | HasFragments => ("f", "")
           };

         switch (Hashtbl.find_opt(dict, key)) {
         | Some(d) =>
           let (actionName, value) = action;
           Hashtbl.add(d, actionName, value);
         | None =>
           let (actionName, value) = action;

           let newDict = Hashtbl.create(1);
           Hashtbl.add(newDict, actionName, value);

           Hashtbl.add(dict, key, newDict);
         };
       });

    dict;
  };

  switch (definition) {
  | Object(obj) =>
    obj.values
    |> traverseProps(
         ~converters,
         ~inRootObject=None,
         ~addInstruction,
         ~currentPath=[],
       )
  | Union(union) =>
    Hashtbl.add(
      converters,
      Printer.makeUnionName(union.atPath),
      switch (direction) {
      | Wrap => Printer.printUnionWrapFnReference(union)
      | Unwrap => Printer.printUnionUnwrapFnReference(union)
      },
    );

    addInstruction({
      atPath: [],
      instruction: ConvertUnion(Printer.makeUnionName(union.atPath)),
    });
    union.members
    |> Tablecloth.List.iter(~f=obj =>
         obj.shape.values
         |> traverseProps(
              ~converters,
              ~inRootObject=None,
              ~addInstruction,
              ~currentPath=[],
            )
       );
  };

  let rootInstructions = makeConverterInstructions(instructions^);

  let converterInstructions = Hashtbl.create(10);

  if (Hashtbl.length(rootInstructions) > 0) {
    Hashtbl.add(converterInstructions, "__root", rootInstructions);
  };

  inputObjectInstructions
  |> Hashtbl.iter((key, entries) => {
       Hashtbl.add(converterInstructions, key, entries)
     });

  {
    converterInstructions,
    convertersDefinition: printConvertersMap(converters),
  };
};

let remove_last_char = str => String.sub(str, 0, String.length(str) - 1);

let printInstrMap = (instrMap: Hashtbl.t(string, string)): string => {
  let attr_list = ref([]);

  instrMap
  |> Hashtbl.iter((key, value) => {
       attr_list := [(key, value), ...attr_list^]
     });

  let instr_map_str =
    attr_list^
    |> List.sort(((keyA, _), (keyB, _)) => String.compare(keyA, keyB))
    |> List.fold_left(
         (acc, (key, value)) => {
           acc ++ Format.sprintf("\"%s\":\"%s\",", key, value)
         },
         "",
       );

  remove_last_char(instr_map_str);
};

let printConverterInstructions =
    (converterInstructions: converterInstructions): string => {
  let str = ref("");
  let addToStr = s => str := str^ ++ s;

  converterInstructions
  |> Hashtbl.iter((path, instrMap) =>
       if (Hashtbl.length(instrMap) > 0) {
         addToStr(Format.sprintf("\"%s\":{", path));
         addToStr(printInstrMap(instrMap));
         addToStr("},");
       }
     );

  remove_last_char(str^);
};

let converterInstructionsToJson =
    (converterInstructions: allConverterInstructions): string => {
  let str = ref("{");
  let addToStr = s => str := str^ ++ s;

  if (Hashtbl.length(converterInstructions) > 0) {
    converterInstructions
    |> Hashtbl.iter((rootKey, instructionMap) =>
         if (Hashtbl.length(instructionMap) > 0) {
           addToStr(Format.sprintf("\"%s\":{", rootKey));
           addToStr(printConverterInstructions(instructionMap));
           addToStr("},");
         }
       );

    str := remove_last_char(str^);
  };

  addToStr("}");

  str^;
};
