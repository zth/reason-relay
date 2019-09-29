open TestFramework;
open ReasonRelayPpxLibrary;

let makeMockLexingPos = (): Stdlib.Lexing.position => {
  pos_fname: "",
  pos_lnum: 0,
  pos_bol: 0,
  pos_cnum: 0,
};

let makeMockLocObj = (): Ppxlib.Location.t => {
  loc_start: makeMockLexingPos(),
  loc_end: makeMockLexingPos(),
  loc_ghost: false,
};

describe("extractTheQueryName", ({test, _}) =>
  test("it should extract the query name", ({expect}) =>
    expect.string(
      Util.extractTheQueryName(
        ~loc=makeMockLocObj(),
        "query SomeQuery { viewer { id } }",
      ),
    ).
      toEqual(
      "SomeQuery",
    )
  )
);

describe("extractTheMutationName", ({test, _}) =>
  test("it should extract the mutation name", ({expect}) =>
    expect.string(
      Util.extractTheMutationName(
        ~loc=makeMockLocObj(),
        "mutation SomeMutation($input: SomeMutationInput!) { someMutation(input: $input) { addedStuff { id } } }",
      ),
    ).
      toEqual(
      "SomeMutation",
    )
  )
);

describe("extractTheFragmentName", ({test, _}) =>
  test("it should extract the fragment name", ({expect}) =>
    expect.string(
      Util.extractTheFragmentName(
        ~loc=makeMockLocObj(),
        "fragment SomeFragment_someProp on SomeEntity { id }",
      ),
    ).
      toEqual(
      "SomeFragment_someProp",
    )
  )
);

describe("extractTheSubscriptionName", ({test, _}) =>
  test("it should extract the subscription name", ({expect}) =>
    expect.string(
      Util.extractTheSubscriptionName(
        ~loc=makeMockLocObj(),
        "subscription SomeSub { viewer { id } }",
      ),
    ).
      toEqual(
      "SomeSub",
    )
  )
);

describe("extractFragmentRefetchableQueryName", ({test, _}) => {
  test(
    "it should extract the refetchable operation query name from a fragment with @refetchable",
    ({expect}) =>
    expect.option(
      Util.extractFragmentRefetchableQueryName(
        ~loc=makeMockLocObj(),
        "fragment SomeFragment_someProp on SomeEntity @refetchable(queryName: \"SomeFragmentRefetchQuery\") { id }",
      ),
    ).
      toBe(
      Some("SomeFragmentRefetchQuery"),
    )
  );

  test(
    "it should extract the refetchable operation query name from a fragment with @refetchable, even if there are multiple directives on the fragment",
    ({expect}) =>
    expect.option(
      Util.extractFragmentRefetchableQueryName(
        ~loc=makeMockLocObj(),
        "fragment SomeFragment_someProp on SomeEntity @argumentDefinitions(id: {type: \"ID!\"}) @refetchable(queryName: \"SomeFragmentRefetchQuery\") { id }",
      ),
    ).
      toBe(
      Some("SomeFragmentRefetchQuery"),
    )
  );
});

describe("fragmentHasConnectionNotation", ({test, _}) => {
  test(
    "it should detect when a fragment has a @connection annotation somewhere",
    ({expect}) =>
    expect.bool(
      Util.fragmentHasConnectionNotation(
        ~loc=makeMockLocObj(),
        {|
        fragment SomeFragment_someProp on SomeEntity @refetchable(queryName: "SomeFragmentRefetchQuery") {
          id
          someConnectionField @connection(key: "SomeFragment_someProp_someConnectionField") {
            edges {
              node {
                id
              }
            }
          }
        }
        |},
      ),
    ).
      toBeTrue()
  );

  test(
    "it should detect when a fragment does not have a @connection annotation",
    ({expect}) =>
    expect.bool(
      Util.fragmentHasConnectionNotation(
        ~loc=makeMockLocObj(),
        {|
        fragment SomeFragment_someProp on SomeEntity @refetchable(queryName: "SomeFragmentRefetchQuery") {
          id
          someConnectionField {
            edges {
              node {
                id
              }
            }
          }
        }
        |},
      ),
    ).
      toBeFalse()
  );
});