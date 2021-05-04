/**
 * This mounts the app to the DOM. Note that the environment is
 * passed to context via <RescriptRelay.Context.Provider /> here -
 * it's a requirement that the environment is available in the
 * context.
 */
ReactExperimental.renderConcurrentRootAtElementWithId(
  <RescriptRelay.Context.Provider environment=RelayEnv.environment>
    <App />
  </RescriptRelay.Context.Provider>,
  "app",
)
