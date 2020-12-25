/**
 * Minimal required binding for subscriptions-transport-ws
 * Note that those bindings are ReasonRelay-specific, and
 * would need to be adjusted in order to be used for
 * other purposes
 */

type operationOptions = {
  query: string,
  variables: Js.Json.t,
}

type observable<'a> = {
  @bs.meth
  "subscribe": ReasonRelay.Observable.sink<'a> => ReasonRelay.Observable.subscription,
}

type t<'a> = {@bs.meth "request": operationOptions => observable<'a>}

@bs.new @bs.module("subscriptions-transport-ws")
external createSubscriptionClient: (string, 'a) => t<'b> = "SubscriptionClient"

let createSubscriptionClient = createSubscriptionClient
