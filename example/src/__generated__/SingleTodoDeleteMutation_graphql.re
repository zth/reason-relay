/* @generated */

module Types = {
  [@ocaml.warning "-30"];
  type response_deleteTodoItem = {deletedTodoItemId: option(string)}
  and rawResponse_deleteTodoItem = {deletedTodoItemId: option(string)}
  and deleteTodoItemInput = {
    id: string,
    clientMutationId: option(string),
  };

  type response = {deleteTodoItem: option(response_deleteTodoItem)};
  type rawResponse = {deleteTodoItem: option(rawResponse_deleteTodoItem)};
  type variables = {
    input: deleteTodoItemInput,
    connections: array(string),
  };
};

module Internal = {
  type wrapResponseRaw;
  let wrapResponseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"deleteTodoItem":{"n":""},"deleteTodoItem_deletedTodoItemId":{"n":""}}} |json}
  ];
  let wrapResponseConverterMap = ();
  let convertWrapResponse = v =>
    v->ReasonRelay.convertObj(
      wrapResponseConverter,
      wrapResponseConverterMap,
      Js.null,
    );

  type responseRaw;
  let responseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"deleteTodoItem":{"n":""},"deleteTodoItem_deletedTodoItemId":{"n":""}}} |json}
  ];
  let responseConverterMap = ();
  let convertResponse = v =>
    v->ReasonRelay.convertObj(
      responseConverter,
      responseConverterMap,
      Js.undefined,
    );

  type wrapRawResponseRaw;
  let wrapRawResponseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"deleteTodoItem":{"n":""},"deleteTodoItem_deletedTodoItemId":{"n":""}}} |json}
  ];
  let wrapRawResponseConverterMap = ();
  let convertWrapRawResponse = v =>
    v->ReasonRelay.convertObj(
      wrapRawResponseConverter,
      wrapRawResponseConverterMap,
      Js.null,
    );

  type rawResponseRaw;
  let rawResponseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"deleteTodoItem":{"n":""},"deleteTodoItem_deletedTodoItemId":{"n":""}}} |json}
  ];
  let rawResponseConverterMap = ();
  let convertRawResponse = v =>
    v->ReasonRelay.convertObj(
      rawResponseConverter,
      rawResponseConverterMap,
      Js.undefined,
    );

  let variablesConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"DeleteTodoItemInput":{"clientMutationId":{"n":""}},"__root":{"input":{"r":"DeleteTodoItemInput"}}} |json}
  ];
  let variablesConverterMap = ();
  let convertVariables = v =>
    v->ReasonRelay.convertObj(
      variablesConverter,
      variablesConverterMap,
      Js.undefined,
    );
};

module Utils = {
  open Types;
  let make_deleteTodoItemInput =
      (~id, ~clientMutationId=?, ()): deleteTodoItemInput => {
    id,
    clientMutationId,
  };

  let makeVariables = (~input, ~connections): variables => {
    input,
    connections,
  };

  let make_rawResponse_deleteTodoItem =
      (~deletedTodoItemId=?, ()): rawResponse_deleteTodoItem => {
    deletedTodoItemId: deletedTodoItemId,
  };

  let make_response_deleteTodoItem =
      (~deletedTodoItemId=?, ()): response_deleteTodoItem => {
    deletedTodoItemId: deletedTodoItemId,
  };

  let makeOptimisticResponse = (~deleteTodoItem=?, ()): rawResponse => {
    deleteTodoItem: deleteTodoItem,
  };
};

type relayOperationNode;

type operationType = ReasonRelay.mutationNode(relayOperationNode);

let node: operationType = [%raw
  {json|(function(){
var v0 = {
  "defaultValue": null,
  "kind": "LocalArgument",
  "name": "connections"
},
v1 = {
  "defaultValue": null,
  "kind": "LocalArgument",
  "name": "input"
},
v2 = [
  {
    "kind": "Variable",
    "name": "input",
    "variableName": "input"
  }
],
v3 = {
  "alias": null,
  "args": null,
  "kind": "ScalarField",
  "name": "deletedTodoItemId",
  "storageKey": null
};
return {
  "fragment": {
    "argumentDefinitions": [
      (v0/*: any*/),
      (v1/*: any*/)
    ],
    "kind": "Fragment",
    "metadata": null,
    "name": "SingleTodoDeleteMutation",
    "selections": [
      {
        "alias": null,
        "args": (v2/*: any*/),
        "concreteType": "DeleteTodoItemPayload",
        "kind": "LinkedField",
        "name": "deleteTodoItem",
        "plural": false,
        "selections": [
          (v3/*: any*/)
        ],
        "storageKey": null
      }
    ],
    "type": "Mutation",
    "abstractKey": null
  },
  "kind": "Request",
  "operation": {
    "argumentDefinitions": [
      (v1/*: any*/),
      (v0/*: any*/)
    ],
    "kind": "Operation",
    "name": "SingleTodoDeleteMutation",
    "selections": [
      {
        "alias": null,
        "args": (v2/*: any*/),
        "concreteType": "DeleteTodoItemPayload",
        "kind": "LinkedField",
        "name": "deleteTodoItem",
        "plural": false,
        "selections": [
          (v3/*: any*/),
          {
            "alias": null,
            "args": null,
            "filters": null,
            "handle": "deleteEdge",
            "key": "",
            "kind": "ScalarHandle",
            "name": "deletedTodoItemId",
            "handleArgs": [
              {
                "kind": "Variable",
                "name": "connections",
                "variableName": "connections"
              }
            ]
          }
        ],
        "storageKey": null
      }
    ]
  },
  "params": {
    "cacheID": "d28fb164d230e5110a2b960f2fedd98f",
    "id": null,
    "metadata": {},
    "name": "SingleTodoDeleteMutation",
    "operationKind": "mutation",
    "text": "mutation SingleTodoDeleteMutation(\n  $input: DeleteTodoItemInput!\n) {\n  deleteTodoItem(input: $input) {\n    deletedTodoItemId\n  }\n}\n"
  }
};
})()|json}
];

[%raw
  {|
(function() {
if (__DEV__) {
  (node/*: any*/).hash = "71791ee7a43554ad19380b72748f1427";
}
})()|}
];

