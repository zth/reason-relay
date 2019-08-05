# master
...

# subscriptions
- BREAKING: Changed signature or `Network.makePromiseBased` from `(fetchFunctionPromise => unit) => Network.t` to `(~fetchFunction: fetchFunctionPromise => unit, ~subscriptionFunction: subscribeFunction=?, ()) => Network.t`.
- Added `Observable` binding.
- Added `Network.makeObserableBased` for creating network layers that use `Observable` from `relay-runtime`.

# 0.1.3
- Add pre-built OS X PPX binary in waiting for a better solution involving Azure Pipelines.

# 0.1.2
- Fix bug with generated fragmentSelector for plural fragments.

# 0.1.1
- Added `resolveNestedRecord` helper to ease resolving records in the store from a nested path.

# 0.1.0-alpha.1
- Fix bugs with `ConnectionHandler` bindings not working.
- Add bindings for getting and setting arrays of primitives as values on `RecordProxy`.
- Move from data-last to data-first to better adhere to what BuckleScript is pushing.
- Add `ReasonRelayUtils` module with helpers for dealing with arrays and connections.
- Add bindings for experimental `getDataID` function on the `Environment`. Won't 
document it for now because the Relay team suggests it's not stable, but it's needed 
to make Relay work with schemas that do not conform to the globally unique IDs 
requirement (Hasura GraphQL for instance).
