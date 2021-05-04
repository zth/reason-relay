/**
 * Here we define our main query. Relay encourages you to split your data demands into
 * multiple fragments in order to make components portable.
 *
 * As you can see below, each component we render that needs data from the server has
 * its own fragment, which is spread on the query. This has a few benefits:
 * - Each component is isolated, and no data leaks between components. No weird bugs when
 *   things suddenly stop working because you thought you knew all places that field you
 *   removed from your query was used.
 * - Relay can optimize and make sure no component unecessarily re-renders, because it
 *   knows exactly what data each component uses, and only updates a component if its
 *   data has been updated.
 *
 * Fragments always have to end up in a query somehow, that's how they get their data.
 * Relay wraps each fragments data in something called a fragment ref. A fragment ref
 * is like a key that can be used to retrieve the data your fragment needs from the Relay
 * store, and is what's left in the object where the data would've been. Each fragment
 * then has a specific React hook that takes any object containing that particular
 * fragments ref, and returns the fragment data.
 *
 * Whew, lots of information... But the only thing you really need to think about is that
 * you _pass along fragment refs by passing the full object the fragment was spread on_.
 */
module Query = %relay(
  `
  query MainQuery {
    siteStatistics {
      ...TopCardsDisplayer_siteStatistics
    }
    ...RecentTickets_query
    ...TodoList_query
  }
`
)

@react.component
let make = () => {
  /**
   * The query definition above automatically generates a React hook for making the query
   * and getting the query data. As mentioned in <App />, this is suspense based, so the
   * loading state needs to be taken care of by wrapping this component with a
   * <React.Suspense />-component.
   *
   * There's a few more things you could pass to the hook here, like `fetchPolicy`, which
   * would control how Relay resolves the data, `cacheConfig` and so on. Please see the
   * documentation for more information.
   */
  let query = Query.use(~variables=(), ())

  <div className="main-panel">
    <div className="content-wrapper">
      <Header />
      <TopCardsDisplayer siteStatistics=query.siteStatistics.fragmentRefs />
      <div className="row">
        <div className="col-8 grid-margin"> <RecentTickets query=query.fragmentRefs /> </div>
        <div className="col-4 grid-margin"> <TodoList query=query.fragmentRefs /> </div>
      </div>
    </div>
  </div>
}
