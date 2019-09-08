module Unions = {};
type variables = unit;
type response = {
  .
  "books":
    array({
      .
      "id": string,
      "__$fragment_ref__BookDisplayer_book": BookDisplayer_book_graphql.t,
    }),
};

let node: ReasonRelay.queryNode = [%bs.raw
  {| (function(){
var v0 = {
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
    "name": "BooksOverviewQuery",
    "type": "Query",
    "metadata": null,
    "argumentDefinitions": [],
    "selections": [
      {
        "kind": "LinkedField",
        "alias": null,
        "name": "books",
        "storageKey": null,
        "args": null,
        "concreteType": "Book",
        "plural": true,
        "selections": [
          (v0/*: any*/),
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
    "name": "BooksOverviewQuery",
    "argumentDefinitions": [],
    "selections": [
      {
        "kind": "LinkedField",
        "alias": null,
        "name": "books",
        "storageKey": null,
        "args": null,
        "concreteType": "Book",
        "plural": true,
        "selections": [
          (v0/*: any*/),
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
              (v0/*: any*/)
            ]
          }
        ]
      }
    ]
  },
  "params": {
    "operationKind": "query",
    "name": "BooksOverviewQuery",
    "id": null,
    "text": "query BooksOverviewQuery {\n  books {\n    id\n    ...BookDisplayer_book\n  }\n}\n\nfragment BookDisplayer_book on Book {\n  ...BookEditor_book\n  title\n  author\n  shelf {\n    ...ShelfDisplayer_shelf\n    id\n  }\n  id\n}\n\nfragment BookEditor_book on Book {\n  id\n  title\n  author\n  status\n}\n\nfragment ShelfDisplayer_shelf on Shelf {\n  name\n}\n",
    "metadata": {}
  }
};
})() |}
];
