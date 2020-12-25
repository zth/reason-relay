/*
 * This mounts the app to the DOM. Note that the environment is
 * passed to context via <ReasonRelay.Context.Provider /> here -
 * it's a requirement that the environment is available in the
 * context.
 */

ReactExperimental.renderConcurrentRootAtElementWithId(
  <ReasonRelay.Context.Provider environment=RelayEnv.environment>
    <App />
  </ReasonRelay.Context.Provider>,
  "app",
)
