/* @generated */

module Types = {
  [@ocaml.warning "-30"];

  type fragment = {
    weeklySales: float,
    weeklyOrders: int,
    currentVisitorsOnline: int,
  };
};

module Internal = {
  type fragmentRaw;
  let fragmentConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {} |json}
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
  ReasonRelay.fragmentRefs([> | `TopCardsDisplayer_siteStatistics]) =>
  fragmentRef =
  "%identity";

module Utils = {};

type relayOperationNode;

type operationType = ReasonRelay.fragmentNode(relayOperationNode);

let node: operationType = [%raw
  {json|{
  "argumentDefinitions": [],
  "kind": "Fragment",
  "metadata": null,
  "name": "TopCardsDisplayer_siteStatistics",
  "selections": [
    {
      "alias": null,
      "args": null,
      "kind": "ScalarField",
      "name": "weeklySales",
      "storageKey": null
    },
    {
      "alias": null,
      "args": null,
      "kind": "ScalarField",
      "name": "weeklyOrders",
      "storageKey": null
    },
    {
      "alias": null,
      "args": null,
      "kind": "ScalarField",
      "name": "currentVisitorsOnline",
      "storageKey": null
    }
  ],
  "type": "SiteStatistics",
  "abstractKey": null
}|json}
];

[%raw
  {|
(function() {
if (__DEV__) {
  (node/*: any*/).hash = "186a09a3710e365aee2da711484e235e";
}
})()|}
];

