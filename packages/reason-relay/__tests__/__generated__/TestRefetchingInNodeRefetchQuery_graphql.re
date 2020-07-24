/* @generated */

type enum_OnlineStatus = [
  | `Idle
  | `Offline
  | `Online
  | `FutureAddedValue(string)
];

let unwrap_enum_OnlineStatus: string => enum_OnlineStatus =
  fun
  | "Idle" => `Idle
  | "Offline" => `Offline
  | "Online" => `Online
  | v => `FutureAddedValue(v);

let wrap_enum_OnlineStatus: enum_OnlineStatus => string =
  fun
  | `Idle => "Idle"
  | `Offline => "Offline"
  | `Online => "Online"
  | `FutureAddedValue(v) => v;

module Types = {
  type response_node = {
    fragmentRefs: ReasonRelay.fragmentRefs([ | `TestRefetchingInNode_user]),
  };

  type response = {node: option(response_node)};
  type rawResponse = response;
  type refetchVariables = {
    friendsOnlineStatuses:
      option(
        array([ | `Idle | `Offline | `Online | `FutureAddedValue(string)]),
      ),
    showOnlineStatus: option(bool),
    id: option(string),
  };
  let makeRefetchVariables =
      (~friendsOnlineStatuses=?, ~showOnlineStatus=?, ~id=?, ())
      : refetchVariables => {
    friendsOnlineStatuses,
    showOnlineStatus,
    id,
  };
  type variables = {
    friendsOnlineStatuses:
      array([ | `Idle | `Offline | `Online | `FutureAddedValue(string)]),
    showOnlineStatus: bool,
    id: string,
  };
};

module Internal = {
  type responseRaw;
  let responseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"node":{"n":"","f":""}}} |json}
  ];
  let responseConverterMap = ();
  let convertResponse = v =>
    v
    ->ReasonRelay._convertObj(
        responseConverter,
        responseConverterMap,
        Js.undefined,
      );

  type rawResponseRaw = responseRaw;
  let convertRawResponse = convertResponse;

  let variablesConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"friendsOnlineStatuses":{"e":"enum_OnlineStatus"}}} |json}
  ];
  let variablesConverterMap = {"enum_OnlineStatus": wrap_enum_OnlineStatus};
  let convertVariables = v =>
    v
    ->ReasonRelay._convertObj(
        variablesConverter,
        variablesConverterMap,
        Js.undefined,
      );
};

type queryRef;

module Utils = {
  open Types;
  let makeVariables =
      (~friendsOnlineStatuses, ~showOnlineStatus, ~id): variables => {
    friendsOnlineStatuses,
    showOnlineStatus,
    id,
  };
};

type operationType = ReasonRelay.queryNode;

let node: operationType = [%raw
  {json| (function(){
var v0 = {
  "defaultValue": [
    "Online",
    "Offline"
  ],
  "kind": "LocalArgument",
  "name": "friendsOnlineStatuses"
},
v1 = {
  "defaultValue": null,
  "kind": "LocalArgument",
  "name": "id"
},
v2 = {
  "defaultValue": false,
  "kind": "LocalArgument",
  "name": "showOnlineStatus"
},
v3 = [
  {
    "kind": "Variable",
    "name": "id",
    "variableName": "id"
  }
];
return {
  "fragment": {
    "argumentDefinitions": [
      (v0/*: any*/),
      (v1/*: any*/),
      (v2/*: any*/)
    ],
    "kind": "Fragment",
    "metadata": null,
    "name": "TestRefetchingInNodeRefetchQuery",
    "selections": [
      {
        "alias": null,
        "args": (v3/*: any*/),
        "concreteType": null,
        "kind": "LinkedField",
        "name": "node",
        "plural": false,
        "selections": [
          {
            "args": [
              {
                "kind": "Variable",
                "name": "friendsOnlineStatuses",
                "variableName": "friendsOnlineStatuses"
              },
              {
                "kind": "Variable",
                "name": "showOnlineStatus",
                "variableName": "showOnlineStatus"
              }
            ],
            "kind": "FragmentSpread",
            "name": "TestRefetchingInNode_user"
          }
        ],
        "storageKey": null
      }
    ],
    "type": "Query",
    "abstractKey": null
  },
  "kind": "Request",
  "operation": {
    "argumentDefinitions": [
      (v0/*: any*/),
      (v2/*: any*/),
      (v1/*: any*/)
    ],
    "kind": "Operation",
    "name": "TestRefetchingInNodeRefetchQuery",
    "selections": [
      {
        "alias": null,
        "args": (v3/*: any*/),
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
            "alias": null,
            "args": null,
            "kind": "ScalarField",
            "name": "id",
            "storageKey": null
          },
          {
            "kind": "InlineFragment",
            "selections": [
              {
                "alias": null,
                "args": null,
                "kind": "ScalarField",
                "name": "firstName",
                "storageKey": null
              },
              {
                "alias": null,
                "args": [
                  {
                    "kind": "Variable",
                    "name": "statuses",
                    "variableName": "friendsOnlineStatuses"
                  }
                ],
                "concreteType": "UserConnection",
                "kind": "LinkedField",
                "name": "friendsConnection",
                "plural": false,
                "selections": [
                  {
                    "alias": null,
                    "args": null,
                    "kind": "ScalarField",
                    "name": "totalCount",
                    "storageKey": null
                  }
                ],
                "storageKey": null
              },
              {
                "condition": "showOnlineStatus",
                "kind": "Condition",
                "passingValue": true,
                "selections": [
                  {
                    "alias": null,
                    "args": null,
                    "kind": "ScalarField",
                    "name": "onlineStatus",
                    "storageKey": null
                  }
                ]
              }
            ],
            "type": "User",
            "abstractKey": null
          }
        ],
        "storageKey": null
      }
    ]
  },
  "params": {
    "cacheID": "04d4234bc755b04365ce0ee2a517f66a",
    "id": null,
    "metadata": {},
    "name": "TestRefetchingInNodeRefetchQuery",
    "operationKind": "query",
    "text": "query TestRefetchingInNodeRefetchQuery(\n  $friendsOnlineStatuses: [OnlineStatus!]! = [Online, Offline]\n  $showOnlineStatus: Boolean! = false\n  $id: ID!\n) {\n  node(id: $id) {\n    __typename\n    ...TestRefetchingInNode_user_lLXHd\n    id\n  }\n}\n\nfragment TestRefetchingInNode_user_lLXHd on User {\n  firstName\n  onlineStatus @include(if: $showOnlineStatus)\n  friendsConnection(statuses: $friendsOnlineStatuses) {\n    totalCount\n  }\n  id\n}\n"
  }
};
})() |json}
];

include ReasonRelay.MakeLoadQuery({
  type variables = Types.variables;
  type loadedQueryRef = queryRef;
  type response = Types.response;
  let query = node;
  let convertVariables = Internal.convertVariables;
});
