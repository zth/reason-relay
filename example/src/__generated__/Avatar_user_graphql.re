/* @generated */

module Types = {
  [@ocaml.warning "-30"];

  type fragment = {
    avatarUrl: option(string),
    fullName: string,
  };
};

module Internal = {
  type fragmentRaw;
  let fragmentConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"avatarUrl":{"n":""}}} |json}
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
  ReasonRelay.fragmentRefs([> | `Avatar_user]) => fragmentRef =
  "%identity";

module Utils = {};

type relayOperationNode;

type operationType = ReasonRelay.fragmentNode(relayOperationNode);

let node: operationType = [%raw
  {json|{
  "argumentDefinitions": [],
  "kind": "Fragment",
  "metadata": null,
  "name": "Avatar_user",
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
}|json}
];

[%raw
  {|
(function() {
if (__DEV__) {
  (node/*: any*/).hash = "de83e3fabd6693c90ab1203d933d0cb0";
}
})()|}
];

