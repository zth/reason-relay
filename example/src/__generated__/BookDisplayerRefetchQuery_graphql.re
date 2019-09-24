module Unions = {};
type variables = {. "id": string};
type response = {
  .
  "node":
    Js.Nullable.t({
      .
      "__$fragment_ref__BookDisplayer_book": BookDisplayer_book_graphql.t,
    }),
};

let node: ReasonRelay.queryNode = [%bs.raw
  {| (function(){
var v0 = [
  {
    "kind": "LocalArgument",
    "name": "id",
    "type": "ID!",
    "defaultValue": null
  }
],
v1 = [
  {
    "kind": "Variable",
    "name": "id",
    "variableName": "id"
  }
],
v2 = {
  "kind": "ScalarField",
  "alias": null,
  "name": "id",
  "args": null,
  "storageKey": null
};
return {
  "kind": "Request",
  "fragment": {
    "kind": "Fragment",
    "name": "BookDisplayerRefetchQuery",
    "type": "Query",
    "metadata": null,
    "argumentDefinitions": (v0/*: any*/),
    "selections": [
      {
        "kind": "LinkedField",
        "alias": null,
        "name": "node",
        "storageKey": null,
        "args": (v1/*: any*/),
        "concreteType": null,
        "plural": false,
        "selections": [
          {
            "kind": "FragmentSpread",
            "name": "BookDisplayer_book",
            "args": null
          }
        ]
      }
    ]
  },
  "operation": {
    "kind": "Operation",
    "name": "BookDisplayerRefetchQuery",
    "argumentDefinitions": (v0/*: any*/),
    "selections": [
      {
        "kind": "LinkedField",
        "alias": null,
        "name": "node",
        "storageKey": null,
        "args": (v1/*: any*/),
        "concreteType": null,
        "plural": false,
        "selections": [
          {
            "kind": "ScalarField",
            "alias": null,
            "name": "__typename",
            "args": null,
            "storageKey": null
          },
          (v2/*: any*/),
          {
            "kind": "InlineFragment",
            "type": "Book",
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
                "kind": "ScalarField",
                "alias": null,
                "name": "status",
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
                    "kind": "ScalarField",
                    "alias": null,
                    "name": "name",
                    "args": null,
                    "storageKey": null
                  },
                  (v2/*: any*/)
                ]
              }
            ]
          }
        ]
      }
    ]
  },
  "params": {
    "operationKind": "query",
    "name": "BookDisplayerRefetchQuery",
    "id": null,
    "text": "query BookDisplayerRefetchQuery(\n  $id: ID!\n) {\n  node(id: $id) {\n    __typename\n    ...BookDisplayer_book\n    id\n  }\n}\n\nfragment BookDisplayer_book on Book {\n  ...BookEditor_book\n  title\n  author\n  shelf {\n    ...ShelfDisplayer_shelf\n    id\n  }\n  id\n}\n\nfragment BookEditor_book on Book {\n  id\n  title\n  author\n  status\n}\n\nfragment ShelfDisplayer_shelf on Shelf {\n  name\n}\n",
    "metadata": {
      "derivedFrom": "BookDisplayer_book",
      "isRefetchableQuery": true
    }
  }
};
})() |}
];
