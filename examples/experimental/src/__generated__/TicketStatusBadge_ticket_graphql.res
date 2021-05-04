/* @generated */
%%raw("/* @generated */")
module Types = {
  @@ocaml.warning("-30")
  
  type enum_TicketStatus = private [>
    | #Done
    | #OnHold
    | #Progress
    | #Rejected
    ]
  
  type enum_TicketStatus_input = [
    | #Done
    | #OnHold
    | #Progress
    | #Rejected
    ]
  
  type fragment = {
    status: enum_TicketStatus,
    dbId: string,
  }
}

module Internal = {
  type fragmentRaw
  let fragmentConverter: 
    Js.Dict.t<Js.Dict.t<Js.Dict.t<string>>> = 
    %raw(
      json`{}`
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
  RescriptRelay.fragmentRefs<[> | #TicketStatusBadge_ticket]> => fragmentRef = "%identity"


module Utils = {
  @@ocaml.warning("-33")
  open Types
  external ticketStatus_toString:
  enum_TicketStatus => string = "%identity"
  external ticketStatus_input_toString:
  enum_TicketStatus_input => string = "%identity"
}
type relayOperationNode
type operationType = RescriptRelay.fragmentNode<relayOperationNode>


let node: operationType = %raw(json` {
  "argumentDefinitions": [],
  "kind": "Fragment",
  "metadata": null,
  "name": "TicketStatusBadge_ticket",
  "selections": [
    {
      "alias": null,
      "args": null,
      "kind": "ScalarField",
      "name": "status",
      "storageKey": null
    },
    {
      "alias": null,
      "args": null,
      "kind": "ScalarField",
      "name": "dbId",
      "storageKey": null
    }
  ],
  "type": "Ticket",
  "abstractKey": null
} `)


