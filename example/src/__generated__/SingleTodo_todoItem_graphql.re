/* @generated */

module Types = {
  [@ocaml.warning "-30"];

  type fragment = {
    id: string,
    text: string,
    completed: option(bool),
  };
};

module Internal = {
  type fragmentRaw;
  let fragmentConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"completed":{"n":""}}} |json}
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
  ReasonRelay.fragmentRefs([> | `SingleTodo_todoItem]) => fragmentRef =
  "%identity";

module Utils = {};

type relayOperationNode;

type operationType = ReasonRelay.fragmentNode(relayOperationNode);

let node: operationType = [%raw
  {json|{
  "argumentDefinitions": [],
  "kind": "Fragment",
  "metadata": null,
  "name": "SingleTodo_todoItem",
  "selections": [
    {
      "alias": null,
      "args": null,
      "kind": "ScalarField",
      "name": "id",
      "storageKey": null
    },
    {
      "alias": null,
      "args": null,
      "kind": "ScalarField",
      "name": "text",
      "storageKey": null
    },
    {
      "alias": null,
      "args": null,
      "kind": "ScalarField",
      "name": "completed",
      "storageKey": null
    }
  ],
  "type": "TodoItem",
  "abstractKey": null
}|json}
];

[%raw
  {|
(function() {
if (__DEV__) {
  (node/*: any*/).hash = "be0bcf1e7e7074845f2b6d7d1f23c376";
}
})()|}
];

