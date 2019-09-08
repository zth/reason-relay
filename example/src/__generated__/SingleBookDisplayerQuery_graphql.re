module Unions = {};
type variables = {. "bookId": string};
type response = {
  .
  "book":
    Js.Nullable.t({
      .
      "__$fragment_ref__BookDisplayer_book": BookDisplayer_book_graphql.t,
      "__$fragment_ref__BookEditor_book": BookEditor_book_graphql.t,
    }),
};

let node: ReasonRelay.queryNode = [%bs.raw
  {| (function(){
var v0 = [
  {
    "kind": "LocalArgument",
    "name": "bookId",
    "type": "ID!",
    "defaultValue": null
  }
],
v1 = [
  {
    "kind": "Variable",
    "name": "id",
    "variableName": "bookId"
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
    "name": "SingleBookDisplayerQuery",
    "type": "Query",
    "metadata": null,
    "argumentDefinitions": (v0/*: any*/),
    "selections": [
      {
        "kind": "LinkedField",
        "alias": null,
        "name": "book",
        "storageKey": null,
        "args": (v1/*: any*/),
        "concreteType": "Book",
        "plural": false,
        "selections": [
          {
            "kind": "FragmentSpread",
            "name": "BookDisplayer_book",
            "args": null
          },
          {
            "kind": "FragmentSpread",
            "name": "BookEditor_book",
            "args": null
          }
        ]
      }
    ]
  },
  "operation": {
    "kind": "Operation",
    "name": "SingleBookDisplayerQuery",
    "argumentDefinitions": (v0/*: any*/),
    "selections": [
      {
        "kind": "LinkedField",
        "alias": null,
        "name": "book",
        "storageKey": null,
        "args": (v1/*: any*/),
        "concreteType": "Book",
        "plural": false,
        "selections": [
          (v2/*: any*/),
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
  },
  "params": {
    "operationKind": "query",
    "name": "SingleBookDisplayerQuery",
    "id": null,
    "text": "query SingleBookDisplayerQuery(\n  $bookId: ID!\n) {\n  book(id: $bookId) {\n    ...BookDisplayer_book\n    ...BookEditor_book\n    id\n  }\n}\n\nfragment BookDisplayer_book on Book {\n  ...BookEditor_book\n  title\n  author\n  shelf {\n    ...ShelfDisplayer_shelf\n    id\n  }\n  id\n}\n\nfragment BookEditor_book on Book {\n  id\n  title\n  author\n  status\n}\n\nfragment ShelfDisplayer_shelf on Shelf {\n  name\n}\n",
    "metadata": {}
  }
};
})() |}
];
