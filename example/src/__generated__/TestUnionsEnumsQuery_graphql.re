module Unions = {
  module Union_fromShelf = {
    type wrapped;

    external __unwrap_union: wrapped => {. "__typename": string} =
      "%identity";
    type type_Book = {
      .
      "id": string,
      "title": string,
      "status": Js.Nullable.t(SchemaAssets.Enum_BookStatus.wrapped),
    };
    type type_BookCollection = {
      .
      "id": string,
      "books":
        Js.Nullable.t(
          array({
            .
            "title": string,
            "status": Js.Nullable.t(SchemaAssets.Enum_BookStatus.wrapped),
          }),
        ),
    };
    external __unwrap_Book: wrapped => type_Book = "%identity";
    external __unwrap_BookCollection: wrapped => type_BookCollection =
      "%identity";

    type t = [
      | `Book(type_Book)
      | `BookCollection(type_BookCollection)
      | `UnmappedUnionMember
    ];

    let unwrap = wrapped => {
      let unwrappedUnion = wrapped |> __unwrap_union;
      switch (unwrappedUnion##__typename) {
      | "Book" => `Book(wrapped |> __unwrap_Book)
      | "BookCollection" =>
        `BookCollection(wrapped |> __unwrap_BookCollection)
      | _ => `UnmappedUnionMember
      };
    };
  };
};
open Unions;
type variables = {
  .
  "bookStatus": SchemaAssets.Enum_BookStatus.wrapped,
  "shelfId": string,
};
type response = {
  .
  "books":
    array({
      .
      "id": string,
      "title": string,
      "status": Js.Nullable.t(SchemaAssets.Enum_BookStatus.wrapped),
    }),
  "fromShelf": Js.Nullable.t(array(Union_fromShelf.wrapped)),
};

let node: ReasonRelay.queryNode = [%bs.raw
  {| (function(){
var v0 = [
  {
    "kind": "LocalArgument",
    "name": "bookStatus",
    "type": "BookStatus!",
    "defaultValue": null
  },
  {
    "kind": "LocalArgument",
    "name": "shelfId",
    "type": "ID!",
    "defaultValue": null
  }
],
v1 = {
  "kind": "ScalarField",
  "alias": null,
  "name": "id",
  "args": null,
  "storageKey": null
},
v2 = {
  "kind": "ScalarField",
  "alias": null,
  "name": "title",
  "args": null,
  "storageKey": null
},
v3 = {
  "kind": "ScalarField",
  "alias": null,
  "name": "status",
  "args": null,
  "storageKey": null
},
v4 = [
  (v1/*: any*/),
  (v2/*: any*/),
  (v3/*: any*/)
],
v5 = {
  "kind": "LinkedField",
  "alias": null,
  "name": "books",
  "storageKey": null,
  "args": [
    {
      "kind": "Variable",
      "name": "status",
      "variableName": "bookStatus"
    }
  ],
  "concreteType": "Book",
  "plural": true,
  "selections": (v4/*: any*/)
},
v6 = [
  {
    "kind": "Variable",
    "name": "shelfId",
    "variableName": "shelfId"
  }
],
v7 = {
  "kind": "ScalarField",
  "alias": null,
  "name": "__typename",
  "args": null,
  "storageKey": null
},
v8 = [
  (v2/*: any*/),
  (v3/*: any*/)
];
return {
  "kind": "Request",
  "fragment": {
    "kind": "Fragment",
    "name": "TestUnionsEnumsQuery",
    "type": "Query",
    "metadata": null,
    "argumentDefinitions": (v0/*: any*/),
    "selections": [
      (v5/*: any*/),
      {
        "kind": "LinkedField",
        "alias": null,
        "name": "fromShelf",
        "storageKey": null,
        "args": (v6/*: any*/),
        "concreteType": null,
        "plural": true,
        "selections": [
          (v7/*: any*/),
          {
            "kind": "InlineFragment",
            "type": "Book",
            "selections": (v4/*: any*/)
          },
          {
            "kind": "InlineFragment",
            "type": "BookCollection",
            "selections": [
              (v1/*: any*/),
              {
                "kind": "LinkedField",
                "alias": null,
                "name": "books",
                "storageKey": null,
                "args": null,
                "concreteType": "Book",
                "plural": true,
                "selections": (v8/*: any*/)
              }
            ]
          }
        ]
      }
    ]
  },
  "operation": {
    "kind": "Operation",
    "name": "TestUnionsEnumsQuery",
    "argumentDefinitions": (v0/*: any*/),
    "selections": [
      (v5/*: any*/),
      {
        "kind": "LinkedField",
        "alias": null,
        "name": "fromShelf",
        "storageKey": null,
        "args": (v6/*: any*/),
        "concreteType": null,
        "plural": true,
        "selections": [
          (v7/*: any*/),
          (v1/*: any*/),
          {
            "kind": "InlineFragment",
            "type": "Book",
            "selections": (v8/*: any*/)
          },
          {
            "kind": "InlineFragment",
            "type": "BookCollection",
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
                  (v2/*: any*/),
                  (v3/*: any*/),
                  (v1/*: any*/)
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
    "name": "TestUnionsEnumsQuery",
    "id": null,
    "text": "query TestUnionsEnumsQuery(\n  $bookStatus: BookStatus!\n  $shelfId: ID!\n) {\n  books(status: $bookStatus) {\n    id\n    title\n    status\n  }\n  fromShelf(shelfId: $shelfId) {\n    __typename\n    ... on Book {\n      id\n      title\n      status\n    }\n    ... on BookCollection {\n      id\n      books {\n        title\n        status\n        id\n      }\n    }\n    ... on Node {\n      id\n    }\n  }\n}\n",
    "metadata": {}
  }
};
})() |}
];
