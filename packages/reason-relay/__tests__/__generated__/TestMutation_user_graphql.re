
/* @generated */

module Types = {
  type enum_OnlineStatus = pri [> | `Idle | `Offline | `Online];

  [@ocaml.warning "-30"];
  type fragment_memberOf_User = {firstName: string};
  type fragment_memberOf_Group = {name: string};
  type fragment_memberOf = [
    | `User(fragment_memberOf_User)
    | `Group(fragment_memberOf_Group)
    | `UnselectedUnionMember(string)
  ];

  type fragment = {
    id: string,
    firstName: string,
    lastName: string,
    onlineStatus: option(enum_OnlineStatus),
    memberOf:
      option(
        array(
          option(
            [
              | `User(fragment_memberOf_User)
              | `Group(fragment_memberOf_Group)
              | `UnselectedUnionMember(string)
            ],
          ),
        ),
      ),
  };
};

let unwrap_fragment_memberOf:
  {. "__typename": string} =>
  [
    | `User(Types.fragment_memberOf_User)
    | `Group(Types.fragment_memberOf_Group)
    | `UnselectedUnionMember(string)
  ] =
  u =>
    switch (u##__typename) {
    | "User" => `User(u->Obj.magic)
    | "Group" => `Group(u->Obj.magic)
    | v => `UnselectedUnionMember(v)
    };

let wrap_fragment_memberOf:
  [
    | `User(Types.fragment_memberOf_User)
    | `Group(Types.fragment_memberOf_Group)
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
    {json| {"__root":{"onlineStatus":{"n":""},"memberOf":{"n":"","na":"","u":"fragment_memberOf"}}} |json}
  ];
  let fragmentConverterMap = {"fragment_memberOf": unwrap_fragment_memberOf};
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
  ReasonRelay.fragmentRefs([> | `TestMutation_user]) => fragmentRef =
  "%identity";

module Utils = {
  external onlineStatus_toString: Types.enum_OnlineStatus => string =
    "%identity";
};

type relayOperationNode;

type operationType = ReasonRelay.fragmentNode(relayOperationNode);



let node: operationType = [%raw {json| (function(){
var v0 = {
  "alias": null,
  "args": null,
  "kind": "ScalarField",
  "name": "firstName",
  "storageKey": null
};
return {
  "argumentDefinitions": [],
  "kind": "Fragment",
  "metadata": null,
  "name": "TestMutation_user",
  "selections": [
    {
      "alias": null,
      "args": null,
      "kind": "ScalarField",
      "name": "id",
      "storageKey": null
    },
    (v0/*: any*/),
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
            (v0/*: any*/)
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
        }
      ],
      "storageKey": null
    }
  ],
  "type": "User",
  "abstractKey": null
};
})() |json}];


