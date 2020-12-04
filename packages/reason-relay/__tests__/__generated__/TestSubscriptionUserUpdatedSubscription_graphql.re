
/* @generated */

module Types = {
  type enum_OnlineStatus = pri [> | `Idle | `Offline | `Online];

  [@ocaml.warning "-30"];
  type response_userUpdated = {user: option(response_userUpdated_user)}
  and response_userUpdated_user = {
    id: string,
    onlineStatus: option(enum_OnlineStatus),
    fragmentRefs: ReasonRelay.fragmentRefs([ | `TestSubscription_user]),
  };

  type response = {userUpdated: option(response_userUpdated)};
  type rawResponse = response;
  type variables = {userId: string};
};

module Internal = {
  type responseRaw;
  let responseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"userUpdated_user_onlineStatus":{"n":""},"userUpdated":{"n":""},"userUpdated_user":{"f":"","n":""}}} |json}
  ];
  let responseConverterMap = ();
  let convertResponse = v =>
    v->ReasonRelay.convertObj(
      responseConverter,
      responseConverterMap,
      Js.undefined,
    );

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

module Utils = {
  external onlineStatus_toString: Types.enum_OnlineStatus => string =
    "%identity";
  open Types;
  let makeVariables = (~userId): variables => {userId: userId};
};

type relayOperationNode;

type operationType = ReasonRelay.subscriptionNode(relayOperationNode);



let node: operationType = [%raw {json| (function(){
var v0 = [
  {
    "defaultValue": null,
    "kind": "LocalArgument",
    "name": "userId"
  }
],
v1 = [
  {
    "kind": "Variable",
    "name": "id",
    "variableName": "userId"
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
  "name": "onlineStatus",
  "storageKey": null
};
return {
  "fragment": {
    "argumentDefinitions": (v0/*: any*/),
    "kind": "Fragment",
    "metadata": null,
    "name": "TestSubscriptionUserUpdatedSubscription",
    "selections": [
      {
        "alias": null,
        "args": (v1/*: any*/),
        "concreteType": "UserUpdatedPayload",
        "kind": "LinkedField",
        "name": "userUpdated",
        "plural": false,
        "selections": [
          {
            "alias": null,
            "args": null,
            "concreteType": "User",
            "kind": "LinkedField",
            "name": "user",
            "plural": false,
            "selections": [
              (v2/*: any*/),
              (v3/*: any*/),
              {
                "args": null,
                "kind": "FragmentSpread",
                "name": "TestSubscription_user"
              }
            ],
            "storageKey": null
          }
        ],
        "storageKey": null
      }
    ],
    "type": "Subscription",
    "abstractKey": null
  },
  "kind": "Request",
  "operation": {
    "argumentDefinitions": (v0/*: any*/),
    "kind": "Operation",
    "name": "TestSubscriptionUserUpdatedSubscription",
    "selections": [
      {
        "alias": null,
        "args": (v1/*: any*/),
        "concreteType": "UserUpdatedPayload",
        "kind": "LinkedField",
        "name": "userUpdated",
        "plural": false,
        "selections": [
          {
            "alias": null,
            "args": null,
            "concreteType": "User",
            "kind": "LinkedField",
            "name": "user",
            "plural": false,
            "selections": [
              (v2/*: any*/),
              (v3/*: any*/),
              {
                "alias": null,
                "args": null,
                "kind": "ScalarField",
                "name": "firstName",
                "storageKey": null
              },
              {
                "alias": null,
                "args": null,
                "kind": "ScalarField",
                "name": "avatarUrl",
                "storageKey": null
              }
            ],
            "storageKey": null
          }
        ],
        "storageKey": null
      }
    ]
  },
  "params": {
    "cacheID": "41b8b12bc049364fa2383c462f7c13de",
    "id": null,
    "metadata": {},
    "name": "TestSubscriptionUserUpdatedSubscription",
    "operationKind": "subscription",
    "text": "subscription TestSubscriptionUserUpdatedSubscription(\n  $userId: ID!\n) {\n  userUpdated(id: $userId) {\n    user {\n      id\n      onlineStatus\n      ...TestSubscription_user\n    }\n  }\n}\n\nfragment TestSubscription_user on User {\n  id\n  firstName\n  avatarUrl\n  onlineStatus\n}\n"
  }
};
})() |json}];


