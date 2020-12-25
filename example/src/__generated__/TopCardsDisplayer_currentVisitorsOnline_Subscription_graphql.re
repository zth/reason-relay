/* @generated */

module Types = {
  [@ocaml.warning "-30"];
  type response_siteStatisticsUpdated = {currentVisitorsOnline: int};

  type response = {
    siteStatisticsUpdated: option(response_siteStatisticsUpdated),
  };
  type rawResponse = response;
  type variables = unit;
};

module Internal = {
  type responseRaw;
  let responseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"siteStatisticsUpdated":{"n":""}}} |json}
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

module Utils = {};

type relayOperationNode;

type operationType = ReasonRelay.subscriptionNode(relayOperationNode);

let node: operationType = [%raw
  {json|(function(){
var v0 = {
  "alias": null,
  "args": null,
  "kind": "ScalarField",
  "name": "currentVisitorsOnline",
  "storageKey": null
};
return {
  "fragment": {
    "argumentDefinitions": [],
    "kind": "Fragment",
    "metadata": null,
    "name": "TopCardsDisplayer_currentVisitorsOnline_Subscription",
    "selections": [
      {
        "alias": null,
        "args": null,
        "concreteType": "SiteStatistics",
        "kind": "LinkedField",
        "name": "siteStatisticsUpdated",
        "plural": false,
        "selections": [
          (v0/*: any*/)
        ],
        "storageKey": null
      }
    ],
    "type": "Subscription",
    "abstractKey": null
  },
  "kind": "Request",
  "operation": {
    "argumentDefinitions": [],
    "kind": "Operation",
    "name": "TopCardsDisplayer_currentVisitorsOnline_Subscription",
    "selections": [
      {
        "alias": null,
        "args": null,
        "concreteType": "SiteStatistics",
        "kind": "LinkedField",
        "name": "siteStatisticsUpdated",
        "plural": false,
        "selections": [
          (v0/*: any*/),
          {
            "alias": null,
            "args": null,
            "kind": "ScalarField",
            "name": "id",
            "storageKey": null
          }
        ],
        "storageKey": null
      }
    ]
  },
  "params": {
    "cacheID": "baa20f0ef1a4dd4e353ad6c271c455bc",
    "id": null,
    "metadata": {
      "subscriptionName": "siteStatisticsUpdated"
    },
    "name": "TopCardsDisplayer_currentVisitorsOnline_Subscription",
    "operationKind": "subscription",
    "text": "subscription TopCardsDisplayer_currentVisitorsOnline_Subscription {\n  siteStatisticsUpdated {\n    currentVisitorsOnline\n    id\n  }\n}\n"
  }
};
})()|json}
];

[%raw
  {|
(function() {
if (__DEV__) {
  (node/*: any*/).hash = "2984ad418eb9ee1f71803def994b7475";
}
})()|}
];

