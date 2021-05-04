/**
 * We wrap <Main /> (which contains our main query) in <React.Suspense />.
 * This is because most Relay hooks APIs are suspense-based, so this is
 * how we handle our loading state.
 */
@react.component
let make = () =>
  <div className="container-scroller">
    <div className="container-fluid page-body-wrapper">
      <React.Suspense fallback={<div> {React.string("Loading...")} </div>}>
        <Main />
      </React.Suspense>
    </div>
  </div>
