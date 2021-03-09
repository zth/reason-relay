---
id: about-rescript-relay
title: About Reason Relay
sidebar_label: About Reason Relay
---

## State of Reason Relay

- Experimental/beta until the point where React and Relay APIs are considered stable
- API changes may occur as we try to iterate and improve on the bindings

## An opinionated binding to Relay

RescriptRelay does _not_ aim to be a 1-to-1 binding of all of Relay. Rather, we take the following approach:

- Focus on binding the parts that lend themselves well to ReasonML's type system. In our case, this means _we only bind Relay's hooks, not their higher-order components_.
- Include tools and utilities that help make life with Relay easier. This includes simplifying common patterns like updating the store, dealing with connections and so on.

This means there's quite a large part of the API surface we won't cover. Rest assured though, what we won't cover will only be _additional ways of doing the same thing_.

## Reason and Relay is actually a great fit

- Type-safety
-
