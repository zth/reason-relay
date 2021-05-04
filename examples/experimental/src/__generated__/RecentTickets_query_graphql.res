/* @generated */
%%raw("/* @generated */")
module Types = {
  @@ocaml.warning("-30")
  
  type rec fragment_ticketsConnection = {
    pageInfo: fragment_ticketsConnection_pageInfo,
    edges: option<array<option<fragment_ticketsConnection_edges>>>,
  }
   and fragment_ticketsConnection_pageInfo = {
    endCursor: option<string>,
    hasNextPage: bool,
  }
   and fragment_ticketsConnection_edges = {
    node: option<fragment_ticketsConnection_edges_node>,
  }
   and fragment_ticketsConnection_edges_node = {
    id: string,
    fragmentRefs: RescriptRelay.fragmentRefs<[ | #SingleTicket_ticket]>
  }
  
  
  type fragment = {
    ticketsConnection: fragment_ticketsConnection,
  }
}

module Internal = {
  type fragmentRaw
  let fragmentConverter: 
    Js.Dict.t<Js.Dict.t<Js.Dict.t<string>>> = 
    %raw(
      json`{"__root":{"ticketsConnection_edges":{"n":"","na":""},"ticketsConnection_pageInfo_endCursor":{"n":""},"ticketsConnection_edges_node":{"f":"","n":""}}}`
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
  RescriptRelay.fragmentRefs<[> | #RecentTickets_query]> => fragmentRef = "%identity"


module Utils = {
  @@ocaml.warning("-33")
  open Types
  @inline
  let connectionKey = "RecentTickets_ticketsConnection"
  
  let getConnectionNodes:
    fragment_ticketsConnection => array<fragment_ticketsConnection_edges_node> =
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


%%private(let makeNode = (node_RecentTicketsRefetchQuery): operationType => {
  ignore(node_RecentTicketsRefetchQuery)
  %raw(json` (function(){
var v0 = [
  "ticketsConnection"
];
return {
  "argumentDefinitions": [
    {
      "defaultValue": "",
      "kind": "LocalArgument",
      "name": "after"
    },
    {
      "defaultValue": 2,
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
        "path": (v0/*: any*/)
      }
    ],
    "refetch": {
      "connection": {
        "forward": {
          "count": "first",
          "cursor": "after"
        },
        "backward": null,
        "path": (v0/*: any*/)
      },
      "fragmentPathInResult": [],
      "operation": node_RecentTicketsRefetchQuery
    }
  },
  "name": "RecentTickets_query",
  "selections": [
    {
      "alias": "ticketsConnection",
      "args": null,
      "concreteType": "TicketConnection",
      "kind": "LinkedField",
      "name": "__RecentTickets_ticketsConnection_connection",
      "plural": false,
      "selections": [
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
          "alias": null,
          "args": null,
          "concreteType": "TicketEdge",
          "kind": "LinkedField",
          "name": "edges",
          "plural": true,
          "selections": [
            {
              "alias": null,
              "args": null,
              "concreteType": "Ticket",
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
                  "name": "SingleTicket_ticket"
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
        }
      ],
      "storageKey": null
    }
  ],
  "type": "Query",
  "abstractKey": null
};
})() `)
})
let node: operationType = makeNode(RecentTicketsRefetchQuery_graphql.node)


