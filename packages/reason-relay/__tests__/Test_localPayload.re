module Query = [%relay.query
  {|
    query TestLocalPayloadQuery @raw_response_type {
      loggedInUser {
        id
        ...TestLocalPayload_user
      }
    }
|}
];

/**
 * Don't mind this fragment, it's mostly here to check that
 * it's actually getting inlined into the types for the query
 * payload we're committing locally below.
 */
module Fragment = [%relay.fragment
  {|
  fragment TestLocalPayload_user on User {
    firstName
    avatarUrl
  }
|}
];

module Test = {
  [@react.component]
  let make = () => {
    let environment = ReasonRelay.useEnvironmentFromContext();
    let data = Query.use(~variables=(), ());
    let user = Fragment.use(data.loggedInUser.fragmentRefs);

    <div>
      <div> {React.string("Firstname: " ++ user.firstName)} </div>
      <div>
        {React.string(
           "Avatar: "
           ++ (
             switch (user.avatarUrl) {
             | Some(avatarUrl) => avatarUrl
             | None => "-"
             }
           ),
         )}
      </div>
      <button
        onClick={_ => {
          Query.commitLocalPayload(
            ~environment,
            ~variables=(),
            ~payload={
              loggedInUser: {
                id: data.loggedInUser.id,
                firstName: "AnotherFirst",
                avatarUrl: None,
              },
            },
          )
        }}>
        {React.string("Update locally")}
      </button>
    </div>;
  };
};

let test_query = () => {
  let network =
    ReasonRelay.Network.makePromiseBased(
      ~fetchFunction=RelayEnv.fetchQuery,
      (),
    );

  let environment =
    ReasonRelay.Environment.make(
      ~network,
      ~store=
        ReasonRelay.Store.make(~source=ReasonRelay.RecordSource.make(), ()),
      (),
    );
  ();

  <TestProviders.Wrapper environment> <Test /> </TestProviders.Wrapper>;
};
