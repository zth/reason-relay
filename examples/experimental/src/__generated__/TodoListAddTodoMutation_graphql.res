/* @generated */
%%raw("/* @generated */")
module Types = {
  @@ocaml.warning("-30")
  
  type rec response_addTodoItem = {
    addedTodoItemEdge: option<response_addTodoItem_addedTodoItemEdge>,
  }
   and response_addTodoItem_addedTodoItemEdge = {
    node: option<response_addTodoItem_addedTodoItemEdge_node>,
  }
   and response_addTodoItem_addedTodoItemEdge_node = {
    id: string,
    text: string,
    completed: option<bool>,
  }
   and rawResponse_addTodoItem = {
    addedTodoItemEdge: option<rawResponse_addTodoItem_addedTodoItemEdge>,
  }
   and rawResponse_addTodoItem_addedTodoItemEdge = {
    node: option<rawResponse_addTodoItem_addedTodoItemEdge_node>,
  }
   and rawResponse_addTodoItem_addedTodoItemEdge_node = {
    id: string,
    text: string,
    completed: option<bool>,
  }
   and addTodoItemInput = {
    text: string,
    clientMutationId: option<string>,
  }
  
  
  type response = {
    addTodoItem: option<response_addTodoItem>,
  }
  type rawResponse = {
    addTodoItem: option<rawResponse_addTodoItem>,
  }
  type variables = {
    input: addTodoItemInput,
    connections: array<RescriptRelay.dataId>,
  }
}

module Internal = {
  type wrapResponseRaw
  let wrapResponseConverter: 
    Js.Dict.t<Js.Dict.t<Js.Dict.t<string>>> = 
    %raw(
      json`{"__root":{"addTodoItem_addedTodoItemEdge_node_completed":{"n":""},"addTodoItem_addedTodoItemEdge_node":{"n":""},"addTodoItem":{"n":""},"addTodoItem_addedTodoItemEdge":{"n":""}}}`
    )
  
  let wrapResponseConverterMap = ()
  let convertWrapResponse = v => v->RescriptRelay.convertObj(
    wrapResponseConverter, 
    wrapResponseConverterMap, 
    Js.null
  )
  type responseRaw
  let responseConverter: 
    Js.Dict.t<Js.Dict.t<Js.Dict.t<string>>> = 
    %raw(
      json`{"__root":{"addTodoItem_addedTodoItemEdge_node_completed":{"n":""},"addTodoItem_addedTodoItemEdge_node":{"n":""},"addTodoItem":{"n":""},"addTodoItem_addedTodoItemEdge":{"n":""}}}`
    )
  
  let responseConverterMap = ()
  let convertResponse = v => v->RescriptRelay.convertObj(
    responseConverter, 
    responseConverterMap, 
    Js.undefined
  )
  type wrapRawResponseRaw
  let wrapRawResponseConverter: 
    Js.Dict.t<Js.Dict.t<Js.Dict.t<string>>> = 
    %raw(
      json`{"__root":{"addTodoItem_addedTodoItemEdge_node_completed":{"n":""},"addTodoItem_addedTodoItemEdge_node":{"n":""},"addTodoItem":{"n":""},"addTodoItem_addedTodoItemEdge":{"n":""}}}`
    )
  
  let wrapRawResponseConverterMap = ()
  let convertWrapRawResponse = v => v->RescriptRelay.convertObj(
    wrapRawResponseConverter, 
    wrapRawResponseConverterMap, 
    Js.null
  )
  type rawResponseRaw
  let rawResponseConverter: 
    Js.Dict.t<Js.Dict.t<Js.Dict.t<string>>> = 
    %raw(
      json`{"__root":{"addTodoItem_addedTodoItemEdge_node_completed":{"n":""},"addTodoItem_addedTodoItemEdge_node":{"n":""},"addTodoItem":{"n":""},"addTodoItem_addedTodoItemEdge":{"n":""}}}`
    )
  
  let rawResponseConverterMap = ()
  let convertRawResponse = v => v->RescriptRelay.convertObj(
    rawResponseConverter, 
    rawResponseConverterMap, 
    Js.undefined
  )
  let variablesConverter: 
    Js.Dict.t<Js.Dict.t<Js.Dict.t<string>>> = 
    %raw(
      json`{"__root":{"input":{"r":"AddTodoItemInput"}},"AddTodoItemInput":{"clientMutationId":{"n":""}}}`
    )
  
  let variablesConverterMap = ()
  let convertVariables = v => v->RescriptRelay.convertObj(
    variablesConverter, 
    variablesConverterMap, 
    Js.undefined
  )
}


module Utils = {
  @@ocaml.warning("-33")
  open Types
  let make_addTodoItemInput = (
    ~text,
    ~clientMutationId=?,
    ()
  ): addTodoItemInput => {
    text: text,
    clientMutationId: clientMutationId
  }
  
  let makeVariables = (
    ~input,
    ~connections
  ): variables => {
    input: input,
    connections: connections
  }
  let make_rawResponse_addTodoItem_addedTodoItemEdge_node = (
    ~id,
    ~text,
    ~completed=?,
    ()
  ): rawResponse_addTodoItem_addedTodoItemEdge_node => {
    id: id,
    text: text,
    completed: completed
  }
  let make_rawResponse_addTodoItem_addedTodoItemEdge = (
    ~node=?,
    ()
  ): rawResponse_addTodoItem_addedTodoItemEdge => {
    node: node
  }
  let make_rawResponse_addTodoItem = (
    ~addedTodoItemEdge=?,
    ()
  ): rawResponse_addTodoItem => {
    addedTodoItemEdge: addedTodoItemEdge
  }
  let make_response_addTodoItem_addedTodoItemEdge_node = (
    ~id,
    ~text,
    ~completed=?,
    ()
  ): response_addTodoItem_addedTodoItemEdge_node => {
    id: id,
    text: text,
    completed: completed
  }
  let make_response_addTodoItem_addedTodoItemEdge = (
    ~node=?,
    ()
  ): response_addTodoItem_addedTodoItemEdge => {
    node: node
  }
  let make_response_addTodoItem = (
    ~addedTodoItemEdge=?,
    ()
  ): response_addTodoItem => {
    addedTodoItemEdge: addedTodoItemEdge
  }
  let makeOptimisticResponse = (
    ~addTodoItem=?,
    ()
  ): rawResponse => {
    addTodoItem: addTodoItem
  }
}
type relayOperationNode
type operationType = RescriptRelay.mutationNode<relayOperationNode>


let node: operationType = %raw(json` (function(){
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
  "concreteType": "TodoItemEdge",
  "kind": "LinkedField",
  "name": "addedTodoItemEdge",
  "plural": false,
  "selections": [
    {
      "alias": null,
      "args": null,
      "concreteType": "TodoItem",
      "kind": "LinkedField",
      "name": "node",
      "plural": false,
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
      "storageKey": null
    }
  ],
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
    "name": "TodoListAddTodoMutation",
    "selections": [
      {
        "alias": null,
        "args": (v2/*: any*/),
        "concreteType": "AddTodoItemPayload",
        "kind": "LinkedField",
        "name": "addTodoItem",
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
    "name": "TodoListAddTodoMutation",
    "selections": [
      {
        "alias": null,
        "args": (v2/*: any*/),
        "concreteType": "AddTodoItemPayload",
        "kind": "LinkedField",
        "name": "addTodoItem",
        "plural": false,
        "selections": [
          (v3/*: any*/),
          {
            "alias": null,
            "args": null,
            "filters": null,
            "handle": "appendEdge",
            "key": "",
            "kind": "LinkedHandle",
            "name": "addedTodoItemEdge",
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
    "cacheID": "24a0ab7c7226072ae69fe6c5dcac2e97",
    "id": null,
    "metadata": {},
    "name": "TodoListAddTodoMutation",
    "operationKind": "mutation",
    "text": "mutation TodoListAddTodoMutation(\n  $input: AddTodoItemInput!\n) {\n  addTodoItem(input: $input) {\n    addedTodoItemEdge {\n      node {\n        id\n        text\n        completed\n      }\n    }\n  }\n}\n"
  }
};
})() `)


