/* @generated */
%%raw("/* @generated */")
module Types = {
  @@ocaml.warning("-30")
  
  type rec fragment_todosConnection = {
    __id: RescriptRelay.dataId,
    edges: option<array<option<fragment_todosConnection_edges>>>,
  }
   and fragment_todosConnection_edges = {
    node: option<fragment_todosConnection_edges_node>,
  }
   and fragment_todosConnection_edges_node = {
    id: string,
    fragmentRefs: RescriptRelay.fragmentRefs<[ | #SingleTodo_todoItem]>
  }
  
  
  type fragment = {
    todosConnection: fragment_todosConnection,
  }
}

module Internal = {
  type fragmentRaw
  let fragmentConverter: 
    Js.Dict.t<Js.Dict.t<Js.Dict.t<string>>> = 
    %raw(
      json`{"__root":{"todosConnection_edges_node":{"f":"","n":""},"todosConnection_edges":{"n":"","na":""}}}`
    )
  
  let fragmentConverterMap = ()
  let convertFragment = v => v->RescriptRelay.convertObj(
    fragmentConverter, 
    fragmentConverterMap, 
    Js.undefined
  )
}
type t
type fragmentRef
external getFragmentRef:
  RescriptRelay.fragmentRefs<[> | #TodoList_query]> => fragmentRef = "%identity"


module Utils = {
  @@ocaml.warning("-33")
  open Types
  @inline
  let connectionKey = "TodoList_query_todosConnection"
  
  let getConnectionNodes:
    fragment_todosConnection => array<fragment_todosConnection_edges_node> =
    connection => switch connection.edges { 
    | None => []
    | Some(edges) => edges->Belt.Array.keepMap(edge => switch edge { 
     | None => None 
     | Some(edge) => edge.node
  
    })
   }
}
type relayOperationNode
type operationType = RescriptRelay.fragmentNode<relayOperationNode>


let node: operationType = %raw(json` {
  "argumentDefinitions": [
    {
      "defaultValue": "",
      "kind": "LocalArgument",
      "name": "after"
    },
    {
      "defaultValue": 10,
      "kind": "LocalArgument",
      "name": "first"
    }
  ],
  "kind": "Fragment",
  "metadata": {
    "connection": [
      {
        "count": "first",
        "cursor": "after",
        "direction": "forward",
        "path": [
          "todosConnection"
        ]
      }
    ]
  },
  "name": "TodoList_query",
  "selections": [
    {
      "alias": "todosConnection",
      "args": null,
      "concreteType": "TodoItemConnection",
      "kind": "LinkedField",
      "name": "__TodoList_query_todosConnection_connection",
      "plural": false,
      "selections": [
        {
          "alias": null,
          "args": null,
          "concreteType": "TodoItemEdge",
          "kind": "LinkedField",
          "name": "edges",
          "plural": true,
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
                  "name": "__typename",
                  "storageKey": null
                },
                {
                  "args": null,
                  "kind": "FragmentSpread",
                  "name": "SingleTodo_todoItem"
                }
              ],
              "storageKey": null
            },
            {
              "alias": null,
              "args": null,
              "kind": "ScalarField",
              "name": "cursor",
              "storageKey": null
            }
          ],
          "storageKey": null
        },
        {
          "alias": null,
          "args": null,
          "concreteType": "PageInfo",
          "kind": "LinkedField",
          "name": "pageInfo",
          "plural": false,
          "selections": [
            {
              "alias": null,
              "args": null,
              "kind": "ScalarField",
              "name": "endCursor",
              "storageKey": null
            },
            {
              "alias": null,
              "args": null,
              "kind": "ScalarField",
              "name": "hasNextPage",
              "storageKey": null
            }
          ],
          "storageKey": null
        },
        {
          "kind": "ClientExtension",
          "selections": [
            {
              "alias": null,
              "args": null,
              "kind": "ScalarField",
              "name": "__id",
              "storageKey": null
            }
          ]
        }
      ],
      "storageKey": null
    }
  ],
  "type": "Query",
  "abstractKey": null
} `)


