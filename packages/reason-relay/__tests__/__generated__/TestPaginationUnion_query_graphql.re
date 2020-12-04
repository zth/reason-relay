
/* @generated */

module Types = {
  [@ocaml.warning "-30"];
  type fragment_members_edges_node_User = {
    id: string,
    fragmentRefs: ReasonRelay.fragmentRefs([ | `TestPaginationUnion_user]),
  };
  type fragment_members_edges_node_Group_adminsConnection_edges_node = {
    id: string,
    firstName: string,
  };
  type fragment_members_edges_node_Group_adminsConnection_edges = {
    node:
      option(fragment_members_edges_node_Group_adminsConnection_edges_node),
  };
  type fragment_members_edges_node_Group_adminsConnection = {
    edges:
      option(
        array(
          option(fragment_members_edges_node_Group_adminsConnection_edges),
        ),
      ),
  };
  type fragment_members_edges_node_Group = {
    adminsConnection: fragment_members_edges_node_Group_adminsConnection,
    name: string,
    id: string,
  };
  type fragment_members_edges_node = [
    | `User(fragment_members_edges_node_User)
    | `Group(fragment_members_edges_node_Group)
    | `UnselectedUnionMember(string)
  ];
  type fragment_members = {
    edges: option(array(option(fragment_members_edges))),
  }
  and fragment_members_edges = {
    node:
      option(
        [
          | `User(fragment_members_edges_node_User)
          | `Group(fragment_members_edges_node_Group)
          | `UnselectedUnionMember(string)
        ],
      ),
  };

  type fragment = {members: option(fragment_members)};
};

let unwrap_fragment_members_edges_node:
  {. "__typename": string} =>
  [
    | `User(Types.fragment_members_edges_node_User)
    | `Group(Types.fragment_members_edges_node_Group)
    | `UnselectedUnionMember(string)
  ] =
  u =>
    switch (u##__typename) {
    | "User" => `User(u->Obj.magic)
    | "Group" => `Group(u->Obj.magic)
    | v => `UnselectedUnionMember(v)
    };

let wrap_fragment_members_edges_node:
  [
    | `User(Types.fragment_members_edges_node_User)
    | `Group(Types.fragment_members_edges_node_Group)
    | `UnselectedUnionMember(string)
  ] =>
  {. "__typename": string} =
  fun
  | `User(v) => v->Obj.magic
  | `Group(v) => v->Obj.magic
  | `UnselectedUnionMember(v) => {"__typename": v};

module Internal = {
  type fragmentRaw;
  let fragmentConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"members_edges_node":{"n":"","u":"fragment_members_edges_node"},"members_edges":{"n":"","na":""},"members_edges_node_group_adminsConnection_edges_node":{"n":""},"members_edges_node_user":{"f":""},"members_edges_node_group_adminsConnection_edges":{"n":"","na":""},"members":{"n":""}}} |json}
  ];
  let fragmentConverterMap = {
    "fragment_members_edges_node": unwrap_fragment_members_edges_node,
  };
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
  ReasonRelay.fragmentRefs([> | `TestPaginationUnion_query]) => fragmentRef =
  "%identity";

module Utils = {
  open Types;
  let getConnectionNodes:
    option(fragment_members) => array(fragment_members_edges_node) =
    connection =>
      switch (connection) {
      | None => [||]
      | Some(connection) =>
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
        }
      };
};

type relayOperationNode;

type operationType = ReasonRelay.fragmentNode(relayOperationNode);



let node: operationType = [%raw {json| (function(){
var v0 = [
  "members"
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
      "name": "groupId"
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
      "fragmentPathInResult": [],
      "operation": require('./TestPaginationUnionRefetchQuery_graphql.bs.js').node
    }
  },
  "name": "TestPaginationUnion_query",
  "selections": [
    {
      "alias": "members",
      "args": [
        {
          "kind": "Variable",
          "name": "groupId",
          "variableName": "groupId"
        },
        {
          "kind": "Variable",
          "name": "onlineStatuses",
          "variableName": "onlineStatuses"
        }
      ],
      "concreteType": "MemberConnection",
      "kind": "LinkedField",
      "name": "__TestPaginationUnion_query_members_connection",
      "plural": false,
      "selections": [
        {
          "alias": null,
          "args": null,
          "concreteType": "MemberEdge",
          "kind": "LinkedField",
          "name": "edges",
          "plural": true,
          "selections": [
            {
              "alias": null,
              "args": null,
              "concreteType": null,
              "kind": "LinkedField",
              "name": "node",
              "plural": false,
              "selections": [
                {
                  "alias": null,
                  "args": null,
                  "kind": "ScalarField",
                  "name": "__typename",
                  "storageKey": null
                },
                {
                  "kind": "InlineFragment",
                  "selections": [
                    (v1/*: any*/),
                    {
                      "args": null,
                      "kind": "FragmentSpread",
                      "name": "TestPaginationUnion_user"
                    }
                  ],
                  "type": "User",
                  "abstractKey": null
                },
                {
                  "kind": "InlineFragment",
                  "selections": [
                    (v1/*: any*/),
                    {
                      "alias": null,
                      "args": null,
                      "kind": "ScalarField",
                      "name": "name",
                      "storageKey": null
                    },
                    {
                      "alias": null,
                      "args": [
                        {
                          "kind": "Literal",
                          "name": "first",
                          "value": 1
                        }
                      ],
                      "concreteType": "UserConnection",
                      "kind": "LinkedField",
                      "name": "adminsConnection",
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
                                  "name": "firstName",
                                  "storageKey": null
                                }
                              ],
                              "storageKey": null
                            }
                          ],
                          "storageKey": null
                        }
                      ],
                      "storageKey": "adminsConnection(first:1)"
                    }
                  ],
                  "type": "Group",
                  "abstractKey": null
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
    }
  ],
  "type": "Query",
  "abstractKey": null
};
})() |json}];


