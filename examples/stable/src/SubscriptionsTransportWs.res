/**
 * Minimal required binding for subscriptions-transport-ws
 * Note that those bindings are RescriptRelay-specific, and
 * would need to be adjusted in order to be used for
 * other purposes
 */
type operationOptions = {
  query: string,
  variables: Js.Json.t,
}

type observable<'a> = {
  @meth
  "subscribe": RescriptRelay.Observable.sink<'a> => RescriptRelay.Observable.subscription,
}

type t<'a> = {@meth "request": operationOptions => observable<'a>}

@new @module("subscriptions-transport-ws")
external createSubscriptionClient: (string, 'a) => t<'b> = "SubscriptionClient"

let createSubscriptionClient = createSubscriptionClient
