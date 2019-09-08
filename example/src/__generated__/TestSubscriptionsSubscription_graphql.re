module Unions = {};
type variables = unit;
type response = {
  .
  "bookAdded": {
    .
    "id": string,
    "title": string,
    "author": string,
  },
};

let node: ReasonRelay.subscriptionNode = [%bs.raw
  {| (function(){
var v0 = [
  {
    "kind": "LinkedField",
    "alias": null,
    "name": "bookAdded",
    "storageKey": null,
    "args": null,
    "concreteType": "Book",
    "plural": false,
    "selections": [
      {
        "kind": "ScalarField",
        "alias": null,
        "name": "id",
        "args": null,
        "storageKey": null
      },
      {
        "kind": "ScalarField",
        "alias": null,
        "name": "title",
        "args": null,
        "storageKey": null
      },
      {
        "kind": "ScalarField",
        "alias": null,
        "name": "author",
        "args": null,
        "storageKey": null
      }
    ]
  }
];
return {
  "kind": "Request",
  "fragment": {
    "kind": "Fragment",
    "name": "TestSubscriptionsSubscription",
    "type": "Subscription",
    "metadata": null,
    "argumentDefinitions": [],
    "selections": (v0/*: any*/)
  },
  "operation": {
    "kind": "Operation",
    "name": "TestSubscriptionsSubscription",
    "argumentDefinitions": [],
    "selections": (v0/*: any*/)
  },
  "params": {
    "operationKind": "subscription",
    "name": "TestSubscriptionsSubscription",
    "id": null,
    "text": "subscription TestSubscriptionsSubscription {\n  bookAdded {\n    id\n    title\n    author\n  }\n}\n",
    "metadata": {}
  }
};
})() |}
];
