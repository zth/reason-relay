/* @generated */

module Types = {
  [@ocaml.warning "-30"];

  type response = {
    fragmentRefs: ReasonRelay.fragmentRefs([ | `RecentTickets_query]),
  };
  type rawResponse = response;
  type refetchVariables = {
    after: option(string),
    first: option(int),
  };
  let makeRefetchVariables = (~after=?, ~first=?, ()): refetchVariables => {
    after,
    first,
  };
  type variables = {
    after: string,
    first: int,
  };
};

module Internal = {
  type wrapResponseRaw;
  let wrapResponseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"":{"f":""}}} |json}
  ];
  let wrapResponseConverterMap = ();
  let convertWrapResponse = v =>
    v->ReasonRelay.convertObj(
      wrapResponseConverter,
      wrapResponseConverterMap,
      Js.null,
    );

  type responseRaw;
  let responseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"":{"f":""}}} |json}
  ];
  let responseConverterMap = ();
  let convertResponse = v =>
    v->ReasonRelay.convertObj(
      responseConverter,
      responseConverterMap,
      Js.undefined,
    );

  type wrapRawResponseRaw = wrapResponseRaw;
  let convertWrapRawResponse = convertWrapResponse;

  type rawResponseRaw = responseRaw;
  let convertRawResponse = convertResponse;

  let variablesConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {} |json}
  ];
  let variablesConverterMap = ();
  let convertVariables = v =>
    v->ReasonRelay.convertObj(
      variablesConverter,
      variablesConverterMap,
      Js.undefined,
    );
};

type queryRef;

module Utils = {
  open Types;
  let makeVariables = (~after, ~first): variables => {after, first};
};

type relayOperationNode;

type operationType = ReasonRelay.queryNode(relayOperationNode);

let node: operationType = [%raw
  {json|(function(){
var v0 = [
  {
    "defaultValue": "",
    "kind": "LocalArgument",
    "name": "after"
  },
  {
    "defaultValue": 2,
    "kind": "LocalArgument",
    "name": "first"
  }
],
v1 = [
  {
    "kind": "Variable",
    "name": "after",
    "variableName": "after"
  },
  {
    "kind": "Variable",
    "name": "first",
    "variableName": "first"
  }
],
v2 = {
  "alias": null,
  "args": null,
  "kind": "ScalarField",
  "name": "id",
  "storageKey": null
},
v3 = {
  "alias": null,
  "args": null,
  "kind": "ScalarField",
  "name": "__typename",
  "storageKey": null
};
return {
  "fragment": {
    "argumentDefinitions": (v0/*: any*/),
    "kind": "Fragment",
    "metadata": null,
    "name": "RecentTicketsRefetchQuery",
    "selections": [
      {
        "args": (v1/*: any*/),
        "kind": "FragmentSpread",
        "name": "RecentTickets_query"
      }
    ],
    "type": "Query",
    "abstractKey": null
  },
  "kind": "Request",
  "operation": {
    "argumentDefinitions": (v0/*: any*/),
    "kind": "Operation",
    "name": "RecentTicketsRefetchQuery",
    "selections": [
      {
        "alias": null,
        "args": (v1/*: any*/),
        "concreteType": "TicketConnection",
        "kind": "LinkedField",
        "name": "ticketsConnection",
        "plural": false,
        "selections": [
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
          },
          {
            "alias": null,
            "args": null,
            "concreteType": "TicketEdge",
            "kind": "LinkedField",
            "name": "edges",
            "plural": true,
            "selections": [
              {
                "alias": null,
                "args": null,
                "concreteType": "Ticket",
                "kind": "LinkedField",
                "name": "node",
                "plural": false,
                "selections": [
                  (v2/*: any*/),
                  {
                    "alias": null,
                    "args": null,
                    "concreteType": null,
                    "kind": "LinkedField",
                    "name": "assignee",
                    "plural": false,
                    "selections": [
                      (v3/*: any*/),
                      {
                        "kind": "InlineFragment",
                        "selections": [
                          {
                            "alias": null,
                            "args": null,
                            "kind": "ScalarField",
                            "name": "avatarUrl",
                            "storageKey": null
                          },
                          {
                            "alias": null,
                            "args": null,
                            "kind": "ScalarField",
                            "name": "fullName",
                            "storageKey": null
                          }
                        ],
                        "type": "User",
                        "abstractKey": null
                      },
                      {
                        "kind": "InlineFragment",
                        "selections": [
                          {
                            "alias": null,
                            "args": null,
                            "kind": "ScalarField",
                            "name": "name",
                            "storageKey": null
                          },
                          (v2/*: any*/)
                        ],
                        "type": "WorkingGroup",
                        "abstractKey": null
                      },
                      {
                        "kind": "InlineFragment",
                        "selections": [
                          (v2/*: any*/)
                        ],
                        "type": "Node",
                        "abstractKey": "__isNode"
                      }
                    ],
                    "storageKey": null
                  },
                  {
                    "alias": null,
                    "args": null,
                    "kind": "ScalarField",
                    "name": "subject",
                    "storageKey": null
                  },
                  {
                    "alias": null,
                    "args": null,
                    "kind": "ScalarField",
                    "name": "lastUpdated",
                    "storageKey": null
                  },
                  {
                    "alias": null,
                    "args": null,
                    "kind": "ScalarField",
                    "name": "trackingId",
                    "storageKey": null
                  },
                  {
                    "alias": null,
                    "args": null,
                    "kind": "ScalarField",
                    "name": "status",
                    "storageKey": null
                  },
                  {
                    "alias": null,
                    "args": null,
                    "kind": "ScalarField",
                    "name": "dbId",
                    "storageKey": null
                  },
                  (v3/*: any*/)
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
          }
        ],
        "storageKey": null
      },
      {
        "alias": null,
        "args": (v1/*: any*/),
        "filters": null,
        "handle": "connection",
        "key": "RecentTickets_ticketsConnection",
        "kind": "LinkedHandle",
        "name": "ticketsConnection"
      }
    ]
  },
  "params": {
    "cacheID": "f1587652ca0eb7dba3ec45d79698549a",
    "id": null,
    "metadata": {},
    "name": "RecentTicketsRefetchQuery",
    "operationKind": "query",
    "text": "query RecentTicketsRefetchQuery(\n  $after: String! = \"\"\n  $first: Int! = 2\n) {\n  ...RecentTickets_query_2HEEH6\n}\n\nfragment Avatar_user on User {\n  avatarUrl\n  fullName\n}\n\nfragment RecentTickets_query_2HEEH6 on Query {\n  ticketsConnection(first: $first, after: $after) {\n    pageInfo {\n      endCursor\n      hasNextPage\n    }\n    edges {\n      node {\n        id\n        ...SingleTicket_ticket\n        __typename\n      }\n      cursor\n    }\n  }\n}\n\nfragment SingleTicketWorkingGroup_workingGroup on WorkingGroup {\n  name\n  id\n}\n\nfragment SingleTicket_ticket on Ticket {\n  assignee {\n    __typename\n    ... on User {\n      ...Avatar_user\n    }\n    ... on WorkingGroup {\n      ...SingleTicketWorkingGroup_workingGroup\n    }\n    ... on Node {\n      __isNode: __typename\n      id\n    }\n  }\n  id\n  subject\n  lastUpdated\n  trackingId\n  ...TicketStatusBadge_ticket\n}\n\nfragment TicketStatusBadge_ticket on Ticket {\n  status\n  dbId\n}\n"
  }
};
})()|json}
];

[%raw
  {|
(function() {
if (__DEV__) {
  (node/*: any*/).hash = "73c22f99343cf0753a8159f984336625";
}
})()|}
];

