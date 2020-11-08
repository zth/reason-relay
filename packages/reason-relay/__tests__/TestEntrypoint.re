module EntryPoint = [%relay.entrypoint
  type queries = {testEntryPoint: TestQuery_graphql.EntryPoint.t}
];

[@react.component]
let make = (~queries) => {
  React.null;
};
