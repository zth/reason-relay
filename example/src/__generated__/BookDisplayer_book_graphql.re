module Unions = {};
type fragment = {
  .
  "title": string,
  "author": string,
  "shelf": {
    .
    "__$fragment_ref__ShelfDisplayer_shelf": ShelfDisplayer_shelf_graphql.t,
  },
  "id": Js.Nullable.t(string),
  "__$fragment_ref__BookEditor_book": BookEditor_book_graphql.t,
};

type t;
type fragmentRef;
type fragmentRefSelector('a) =
  {.. "__$fragment_ref__BookDisplayer_book": t} as 'a;
external getFragmentRef: fragmentRefSelector('a) => fragmentRef = "%identity";

let node: ReasonRelay.fragmentNode = [%bs.raw
  {| {
  "kind": "Fragment",
  "name": "BookDisplayer_book",
  "type": "Book",
  "metadata": {
    "refetch": {
      "connection": null,
      "operation": require('./BookDisplayerRefetchQuery_graphql.bs.js'),
      "fragmentPathInResult": [
        "node"
      ]
    }
  },
  "argumentDefinitions": [],
  "selections": [
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
    },
    {
      "kind": "LinkedField",
      "alias": null,
      "name": "shelf",
      "storageKey": null,
      "args": null,
      "concreteType": "Shelf",
      "plural": false,
      "selections": [
        {
          "kind": "FragmentSpread",
          "name": "ShelfDisplayer_shelf",
          "args": null
        }
      ]
    },
    {
      "kind": "ScalarField",
      "alias": null,
      "name": "id",
      "args": null,
      "storageKey": null
    },
    {
      "kind": "FragmentSpread",
      "name": "BookEditor_book",
      "args": null
    }
  ]
} |}
];
