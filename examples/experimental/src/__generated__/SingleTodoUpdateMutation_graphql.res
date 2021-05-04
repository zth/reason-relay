/* @generated */
%%raw("/* @generated */")
module Types = {
  @@ocaml.warning("-30")
  
  type rec response_updateTodoItem = {
    updatedTodoItem: option<response_updateTodoItem_updatedTodoItem>,
  }
   and response_updateTodoItem_updatedTodoItem = {
    id: string,
    text: string,
    completed: option<bool>,
  }
   and updateTodoItemInput = {
    id: string,
    text: string,
    completed: bool,
    clientMutationId: option<string>,
  }
  
  
  type response = {
    updateTodoItem: option<response_updateTodoItem>,
  }
  type rawResponse = response
  type variables = {
    input: updateTodoItemInput,
  }
}

module Internal = {
  type wrapResponseRaw
  let wrapResponseConverter: 
    Js.Dict.t<Js.Dict.t<Js.Dict.t<string>>> = 
    %raw(
      json`{"__root":{"updateTodoItem_updatedTodoItem_completed":{"n":""},"updateTodoItem_updatedTodoItem":{"n":""},"updateTodoItem":{"n":""}}}`
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
      json`{"__root":{"updateTodoItem_updatedTodoItem_completed":{"n":""},"updateTodoItem_updatedTodoItem":{"n":""},"updateTodoItem":{"n":""}}}`
    )
  
  let responseConverterMap = ()
  let convertResponse = v => v->RescriptRelay.convertObj(
    responseConverter, 
    responseConverterMap, 
    Js.undefined
  )
  type wrapRawResponseRaw = wrapResponseRaw
  let convertWrapRawResponse = convertWrapResponse
  type rawResponseRaw = responseRaw
  let convertRawResponse = convertResponse
  let variablesConverter: 
    Js.Dict.t<Js.Dict.t<Js.Dict.t<string>>> = 
    %raw(
      json`{"UpdateTodoItemInput":{"clientMutationId":{"n":""}},"__root":{"input":{"r":"UpdateTodoItemInput"}}}`
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
  let make_updateTodoItemInput = (
    ~id,
    ~text,
    ~completed,
    ~clientMutationId=?,
    ()
  ): updateTodoItemInput => {
    id: id,
    text: text,
    completed: completed,
    clientMutationId: clientMutationId
  }
  
  let makeVariables = (
    ~input
  ): variables => {
    input: input
  }
  let make_response_updateTodoItem_updatedTodoItem = (
    ~id,
    ~text,
    ~completed=?,
    ()
  ): response_updateTodoItem_updatedTodoItem => {
    id: id,
    text: text,
    completed: completed
  }
  let make_response_updateTodoItem = (
    ~updatedTodoItem=?,
    ()
  ): response_updateTodoItem => {
    updatedTodoItem: updatedTodoItem
  }
  let makeOptimisticResponse = (
    ~updateTodoItem=?,
    ()
  ): rawResponse => {
    updateTodoItem: updateTodoItem
  }
}
type relayOperationNode
type operationType = RescriptRelay.mutationNode<relayOperationNode>


let node: operationType = %raw(json` (function(){
var v0 = [
  {
    "defaultValue": null,
    "kind": "LocalArgument",
    "name": "input"
  }
],
v1 = [
  {
    "alias": null,
    "args": [
      {
        "kind": "Variable",
        "name": "input",
        "variableName": "input"
      }
    ],
    "concreteType": "UpdateTodoItemPayload",
    "kind": "LinkedField",
    "name": "updateTodoItem",
    "plural": false,
    "selections": [
      {
        "alias": null,
        "args": null,
        "concreteType": "TodoItem",
        "kind": "LinkedField",
        "name": "updatedTodoItem",
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
  }
];
return {
  "fragment": {
    "argumentDefinitions": (v0/*: any*/),
    "kind": "Fragment",
    "metadata": null,
    "name": "SingleTodoUpdateMutation",
    "selections": (v1/*: any*/),
    "type": "Mutation",
    "abstractKey": null
  },
  "kind": "Request",
  "operation": {
    "argumentDefinitions": (v0/*: any*/),
    "kind": "Operation",
    "name": "SingleTodoUpdateMutation",
    "selections": (v1/*: any*/)
  },
  "params": {
    "cacheID": "8481abbb88754162a4bd7301d1150105",
    "id": null,
    "metadata": {},
    "name": "SingleTodoUpdateMutation",
    "operationKind": "mutation",
    "text": "mutation SingleTodoUpdateMutation(\n  $input: UpdateTodoItemInput!\n) {\n  updateTodoItem(input: $input) {\n    updatedTodoItem {\n      id\n      text\n      completed\n    }\n  }\n}\n"
  }
};
})() `)


