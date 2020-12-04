
/* @generated */

module Types = {
  type enum_OnlineStatus = pri [> | `Idle | `Offline | `Online];

  [@ocaml.warning "-30"];
  type rawResponse_setOnlineStatus_user_memberOf_User = {
    firstName: string,
    id: string,
    __isNode: [ | `User],
  };
  type rawResponse_setOnlineStatus_user_memberOf_Group = {
    name: string,
    id: string,
    __isNode: [ | `Group],
  };
  type rawResponse_setOnlineStatus_user_memberOf = [
    | `User(rawResponse_setOnlineStatus_user_memberOf_User)
    | `Group(rawResponse_setOnlineStatus_user_memberOf_Group)
    | `UnselectedUnionMember(string)
  ];
  type response_setOnlineStatus = {
    user: option(response_setOnlineStatus_user),
  }
  and response_setOnlineStatus_user = ReasonRelay.allFieldsMasked
  and rawResponse_setOnlineStatus = {
    user: option(rawResponse_setOnlineStatus_user),
  }
  and rawResponse_setOnlineStatus_user = {
    id: string,
    firstName: string,
    lastName: string,
    onlineStatus: option(enum_OnlineStatus),
    memberOf:
      option(
        array(
          option(
            [
              | `User(rawResponse_setOnlineStatus_user_memberOf_User)
              | `Group(rawResponse_setOnlineStatus_user_memberOf_Group)
              | `UnselectedUnionMember(string)
            ],
          ),
        ),
      ),
  };

  type response = {setOnlineStatus: option(response_setOnlineStatus)};
  type rawResponse = {setOnlineStatus: option(rawResponse_setOnlineStatus)};
  type variables = {onlineStatus: enum_OnlineStatus};
};

let unwrap_rawResponse_setOnlineStatus_user_memberOf:
  {. "__typename": string} =>
  [
    | `User(Types.rawResponse_setOnlineStatus_user_memberOf_User)
    | `Group(Types.rawResponse_setOnlineStatus_user_memberOf_Group)
    | `UnselectedUnionMember(string)
  ] =
  u =>
    switch (u##__typename) {
    | "User" => `User(u->Obj.magic)
    | "Group" => `Group(u->Obj.magic)
    | v => `UnselectedUnionMember(v)
    };

let wrap_rawResponse_setOnlineStatus_user_memberOf:
  [
    | `User(Types.rawResponse_setOnlineStatus_user_memberOf_User)
    | `Group(Types.rawResponse_setOnlineStatus_user_memberOf_Group)
    | `UnselectedUnionMember(string)
  ] =>
  {. "__typename": string} =
  fun
  | `User(v) => v->Obj.magic
  | `Group(v) => v->Obj.magic
  | `UnselectedUnionMember(v) => {"__typename": v};

module Internal = {
  type wrapResponseRaw;
  let wrapResponseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"setOnlineStatus":{"n":""},"setOnlineStatus_user":{"f":"","n":""}}} |json}
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
    {json| {"__root":{"setOnlineStatus":{"n":""},"setOnlineStatus_user":{"f":"","n":""}}} |json}
  ];
  let responseConverterMap = ();
  let convertResponse = v =>
    v->ReasonRelay.convertObj(
      responseConverter,
      responseConverterMap,
      Js.undefined,
    );

  type wrapRawResponseRaw;
  let wrapRawResponseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"setOnlineStatus":{"n":""},"setOnlineStatus_user_memberOf":{"n":"","na":"","u":"rawResponse_setOnlineStatus_user_memberOf"},"setOnlineStatus_user_onlineStatus":{"n":""},"setOnlineStatus_user":{"n":""}}} |json}
  ];
  let wrapRawResponseConverterMap = {
    "rawResponse_setOnlineStatus_user_memberOf": wrap_rawResponse_setOnlineStatus_user_memberOf,
  };
  let convertWrapRawResponse = v =>
    v->ReasonRelay.convertObj(
      wrapRawResponseConverter,
      wrapRawResponseConverterMap,
      Js.null,
    );

  type rawResponseRaw;
  let rawResponseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"setOnlineStatus":{"n":""},"setOnlineStatus_user_memberOf":{"n":"","na":"","u":"rawResponse_setOnlineStatus_user_memberOf"},"setOnlineStatus_user_onlineStatus":{"n":""},"setOnlineStatus_user":{"n":""}}} |json}
  ];
  let rawResponseConverterMap = {
    "rawResponse_setOnlineStatus_user_memberOf": unwrap_rawResponse_setOnlineStatus_user_memberOf,
  };
  let convertRawResponse = v =>
    v->ReasonRelay.convertObj(
      rawResponseConverter,
      rawResponseConverterMap,
      Js.undefined,
    );

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
  let makeVariables = (~onlineStatus): variables => {
    onlineStatus: onlineStatus,
  };

  let make_rawResponse_setOnlineStatus_user =
      (~id, ~firstName, ~lastName, ~onlineStatus=?, ~memberOf=?, ())
      : rawResponse_setOnlineStatus_user => {
    id,
    firstName,
    lastName,
    onlineStatus,
    memberOf,
  };

  let make_rawResponse_setOnlineStatus =
      (~user=?, ()): rawResponse_setOnlineStatus => {
    user: user,
  };

  let make_response_setOnlineStatus_user = () => [@ocaml.warning "-27"] {};

  let make_response_setOnlineStatus = (~user=?, ()): response_setOnlineStatus => {
    user: user,
  };

  let makeOptimisticResponse = (~setOnlineStatus=?, ()): rawResponse => {
    setOnlineStatus: setOnlineStatus,
  };
};

type relayOperationNode;

type operationType = ReasonRelay.mutationNode(relayOperationNode);



let node: operationType = [%raw {json| (function(){
var v0 = [
  {
    "defaultValue": null,
    "kind": "LocalArgument",
    "name": "onlineStatus"
  }
],
v1 = [
  {
    "kind": "Variable",
    "name": "onlineStatus",
    "variableName": "onlineStatus"
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
  "name": "firstName",
  "storageKey": null
};
return {
  "fragment": {
    "argumentDefinitions": (v0/*: any*/),
    "kind": "Fragment",
    "metadata": null,
    "name": "TestMutationWithOnlyFragmentSetOnlineStatusMutation",
    "selections": [
      {
        "alias": null,
        "args": (v1/*: any*/),
        "concreteType": "SetOnlineStatusPayload",
        "kind": "LinkedField",
        "name": "setOnlineStatus",
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
              {
                "args": null,
                "kind": "FragmentSpread",
                "name": "TestMutation_user"
              }
            ],
            "storageKey": null
          }
        ],
        "storageKey": null
      }
    ],
    "type": "Mutation",
    "abstractKey": null
  },
  "kind": "Request",
  "operation": {
    "argumentDefinitions": (v0/*: any*/),
    "kind": "Operation",
    "name": "TestMutationWithOnlyFragmentSetOnlineStatusMutation",
    "selections": [
      {
        "alias": null,
        "args": (v1/*: any*/),
        "concreteType": "SetOnlineStatusPayload",
        "kind": "LinkedField",
        "name": "setOnlineStatus",
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
                "name": "lastName",
                "storageKey": null
              },
              {
                "alias": null,
                "args": null,
                "kind": "ScalarField",
                "name": "onlineStatus",
                "storageKey": null
              },
              {
                "alias": null,
                "args": null,
                "concreteType": null,
                "kind": "LinkedField",
                "name": "memberOf",
                "plural": true,
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
                      (v3/*: any*/)
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
                      }
                    ],
                    "type": "Group",
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
    "cacheID": "e0feba611b52ad3b8ccb480d649750c3",
    "id": null,
    "metadata": {},
    "name": "TestMutationWithOnlyFragmentSetOnlineStatusMutation",
    "operationKind": "mutation",
    "text": "mutation TestMutationWithOnlyFragmentSetOnlineStatusMutation(\n  $onlineStatus: OnlineStatus!\n) {\n  setOnlineStatus(onlineStatus: $onlineStatus) {\n    user {\n      ...TestMutation_user\n      id\n    }\n  }\n}\n\nfragment TestMutation_user on User {\n  id\n  firstName\n  lastName\n  onlineStatus\n  memberOf {\n    __typename\n    ... on User {\n      firstName\n    }\n    ... on Group {\n      name\n    }\n    ... on Node {\n      __isNode: __typename\n      id\n    }\n  }\n}\n"
  }
};
})() |json}];


