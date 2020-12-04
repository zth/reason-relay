
/* @generated */

module Types = {
  [@ocaml.warning "-30"];
  type fragment_friendsConnection = {
    edges: option(array(option(fragment_friendsConnection_edges))),
  }
  and fragment_friendsConnection_edges = {
    node: option(fragment_friendsConnection_edges_node),
  }
  and fragment_friendsConnection_edges_node = {
    id: string,
    fragmentRefs: ReasonRelay.fragmentRefs([ | `TestPaginationInNode_user]),
  };

  type fragment = {
    friendsConnection: fragment_friendsConnection,
    id: string,
  };
};

module Internal = {
  type fragmentRaw;
  let fragmentConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"friendsConnection_edges":{"n":"","na":""},"friendsConnection_edges_node":{"f":"","n":""}}} |json}
  ];
  let fragmentConverterMap = ();
  let convertFragment = v =>
    v->ReasonRelay.convertObj(
      fragmentConverter,
      fragmentConverterMap,
      Js.undefined,
    );
};

type t;
type fragmentRef;
external getFragmentRef:
  ReasonRelay.fragmentRefs([> | `TestPaginationInNode_query]) => fragmentRef =
  "%identity";

module Utils = {
  open Types;
  let getConnectionNodes:
    fragment_friendsConnection => array(fragment_friendsConnection_edges_node) =
    connection =>
      switch (connection.edges) {
      | None => [||]
      | Some(edges) =>
        edges->Belt.Array.keepMap(edge =>
          switch (edge) {
          | None => None
          | Some(edge) =>
            switch (edge.node) {
            | None => None
            | Some(node) => Some(node)
            }
          }
        )
      };
};

type relayOperationNode;

type operationType = ReasonRelay.fragmentNode(relayOperationNode);



let node: operationType = [%raw {json| (function(){
var v0 = [
  "friendsConnection"
],
v1 = {
  "alias": null,
  "args": null,
  "kind": "ScalarField",
  "name": "id",
  "storageKey": null
};
return {
  "argumentDefinitions": [
    {
      "defaultValue": 2,
      "kind": "LocalArgument",
      "name": "count"
    },
    {
      "defaultValue": "",
      "kind": "LocalArgument",
      "name": "cursor"
    },
    {
      "defaultValue": null,
      "kind": "LocalArgument",
      "name": "onlineStatuses"
    }
  ],
  "kind": "Fragment",
  "metadata": {
    "connection": [
      {
        "count": "count",
        "cursor": "cursor",
        "direction": "forward",
        "path": (v0/*: any*/)
      }
    ],
    "refetch": {
      "connection": {
        "forward": {
          "count": "count",
          "cursor": "cursor"
        },
        "backward": null,
        "path": (v0/*: any*/)
      },
      "fragmentPathInResult": [
        "node"
      ],
      "operation": require('./TestPaginationInNodeRefetchQuery_graphql.bs.js').node,
      "identifierField": "id"
    }
  },
  "name": "TestPaginationInNode_query",
  "selections": [
    {
      "alias": "friendsConnection",
      "args": [
        {
          "kind": "Variable",
          "name": "statuses",
          "variableName": "onlineStatuses"
        }
      ],
      "concreteType": "UserConnection",
      "kind": "LinkedField",
      "name": "__TestPaginationInNode_friendsConnection_connection",
      "plural": false,
      "selections": [
        {
          "alias": null,
          "args": null,
          "concreteType": "UserEdge",
          "kind": "LinkedField",
          "name": "edges",
          "plural": true,
          "selections": [
            {
              "alias": null,
              "args": null,
              "concreteType": "User",
              "kind": "LinkedField",
              "name": "node",
              "plural": false,
              "selections": [
                (v1/*: any*/),
                {
                  "alias": null,
                  "args": null,
                  "kind": "ScalarField",
                  "name": "__typename",
                  "storageKey": null
                },
                {
                  "args": null,
                  "kind": "FragmentSpread",
                  "name": "TestPaginationInNode_user"
                }
              ],
              "storageKey": null
            },
            {
              "alias": null,
              "args": null,
              "kind": "ScalarField",
              "name": "cursor",
              "storageKey": null
            }
          ],
          "storageKey": null
        },
        {
          "alias": null,
          "args": null,
          "concreteType": "PageInfo",
          "kind": "LinkedField",
          "name": "pageInfo",
          "plural": false,
          "selections": [
            {
              "alias": null,
              "args": null,
              "kind": "ScalarField",
              "name": "endCursor",
              "storageKey": null
            },
            {
              "alias": null,
              "args": null,
              "kind": "ScalarField",
              "name": "hasNextPage",
              "storageKey": null
            }
          ],
          "storageKey": null
        }
      ],
      "storageKey": null
    },
    (v1/*: any*/)
  ],
  "type": "User",
  "abstractKey": null
};
})() |json}];


