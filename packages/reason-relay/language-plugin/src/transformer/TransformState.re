/**
 This transforms the intermediate state produced by the type parser into
 a full state with all parts we need, like enums, unions etc.
*/

let intermediateToFull =
    (intermediateState: Types.intermediateState): Types.fullState => {
  let initialState: Types.fullState = {
    enums: intermediateState.enums,
    unions: [],
    objects:
      intermediateState.objects
      |> List.map((obj: Types.obj) =>
           (
             {
               originalFlowTypeName: obj.originalFlowTypeName,
               recordName: obj.originalFlowTypeName,
               atPath: ["root"],
               definition: obj.definition,
               foundInUnion: obj.foundInUnion,
             }: Types.finalizedObj
           )
         ),
    variables:
      switch (intermediateState.variables) {
      | Some(v) => Some(v.definition)
      | None => None
      },
    response:
      switch (intermediateState.response) {
      | Some(v) => Some(v.definition)
      | None => None
      },
    rawResponse:
      switch (intermediateState.rawResponse) {
      | Some(v) => Some(v.definition)
      | None => None
      },
    fragment: intermediateState.fragment,
  };

  let state = ref(initialState);

  let setState = updater => state := updater(state^);
  let addEnum = enum => setState(s => {...s, enums: [enum, ...s.enums]});
  let addUnion = union =>
    setState(s => {...s, unions: [union, ...s.unions]});
  let addObject = obj => setState(s => {...s, objects: [obj, ...s.objects]});

  let rec traverseDefinition =
          (~inUnion, ~atPath: list(string), definition: Types.object_) =>
    definition.values |> Array.iter(traversePropValue(~inUnion, ~atPath))
  and traversePropValue =
      (~inUnion, ~atPath: list(string), propValue: Types.propValues) =>
    switch (propValue) {
    | FragmentRef(_) => ()
    | Prop(name, {propType}) =>
      let newAtPath = [name, ...atPath];

      switch (propType) {
      | Array({propType: Enum(enum)})
      | Enum(enum) => addEnum(enum)
      | Array({propType: Union(union)})
      | Union(union) =>
        addUnion({union, printName: true});

        union.members
        ->Belt.List.forEach(member => {
            member.shape
            |> traverseDefinition(
                 ~inUnion=true,
                 ~atPath=[member.name, ...newAtPath],
               )
          });
      | Array({propType: Object(definition)})
      | TopLevelNodeField(_, definition)
      | Object(definition) =>
        addObject({
          atPath: newAtPath,
          recordName: None,
          originalFlowTypeName: None,
          definition,
          foundInUnion: inUnion,
        });
        definition |> traverseDefinition(~inUnion, ~atPath=newAtPath);
      | DataId
      | Array(_)
      | Scalar(_)
      | FragmentRefValue(_)
      | TypeReference(_) => ()
      };
    };

  switch (state^.variables) {
  | Some(d) => d |> traverseDefinition(~inUnion=false, ~atPath=["variables"])
  | None => ()
  };

  switch (state^.response) {
  | Some(d) => d |> traverseDefinition(~inUnion=false, ~atPath=["response"])
  | None => ()
  };

  switch (state^.rawResponse) {
  | Some(d) =>
    d |> traverseDefinition(~inUnion=false, ~atPath=["rawResponse"])
  | None => ()
  };

  switch (state^.fragment) {
  | Some({definition: Object(obj)}) =>
    obj |> traverseDefinition(~inUnion=false, ~atPath=["fragment"])
  | Some({definition: Union(union)}) =>
    addUnion({union, printName: false});
    union.members
    ->Belt.List.forEach(member => {
        member.shape |> traverseDefinition(~inUnion=true, ~atPath=[])
      });
  | None => ()
  };

  setState(s =>
    {
      ...s,
      objects:
        s.objects
        |> List.map((obj: Types.finalizedObj) => {
             let recordName =
               switch (obj.recordName) {
               | None => Some(Utils.makeRecordName(obj.atPath))
               | s => s
               };

             {...obj, recordName};
           }),
    }
  );

  {
    ...state^,
    enums:
      state^.enums
      |> Tablecloth.List.uniqueBy(~f=(e: Types.fullEnum) => e.name),
  };
};
