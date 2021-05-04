/**
 * Here we define a fragment, telling Relay that if this component is to be
 * rendered, it needs the following fields on the type SiteStatistics.
 *
 * This is a pretty basic fragment, there are more complex ones you can have
 * a look at in other components.
 */
module SiteStatisticsFragment = %relay(`
  fragment TopCardsDisplayer_siteStatistics on SiteStatistics {
    weeklySales
    weeklyOrders
    currentVisitorsOnline
  }
`)

module CurrentVisitorsSubscription = %relay(`
  subscription TopCardsDisplayer_currentVisitorsOnline_Subscription {
    siteStatisticsUpdated {
      currentVisitorsOnline
    }
  }
`)

/**
 * A few things to note about the following component:
 * - I rename siteStatistics to siteStatisticsRef, which is purely because I
 *   feel it's more evident what it is (and that I want to use siteStatistics
 *   as name for the data I extract via the fragment) and is in no way
 *   required.
 * - %relay() autogenerates a `use` hook, which takes the object holding
 *   the fragment ref and extracts the data. Thanks to the superpowers of inference,
 *   we don't really need to annotate anything here for it to be 100% type safe.
 *
 */
@react.component
let make = (~siteStatistics as siteStatisticsRef) => {
  let siteStatistics = SiteStatisticsFragment.use(siteStatisticsRef)

  let environment = RescriptRelay.useEnvironmentFromContext()
  React.useEffect0(() => {
    let subscription = CurrentVisitorsSubscription.subscribe(
      ~environment,
      ~variables=(),
      ~onNext=response =>
        switch response.siteStatisticsUpdated {
        | Some(siteStatisticsUpdated) =>
          /* Console-logging the response for demo purposes
           Note that the store (and thus the UI) gets updated "automatically" */
          Js.log2(
            "Subscription response - current visitors online: ",
            siteStatisticsUpdated.currentVisitorsOnline,
          )
        | None => ()
        },
      (),
    )

    Some(() => RescriptRelay.Disposable.dispose(subscription))
  })

  <div className="row">
    <div className="col-md-4 stretch-card grid-margin">
      <EmphasizedCard
        title="Weekly Sales"
        subText={"$ " ++ Js.Float.toString(siteStatistics.weeklySales)}
        icon=EmphasizedCard.ChartLine
        variant=EmphasizedCard.Red
      />
    </div>
    <div className="col-md-4 stretch-card grid-margin">
      <EmphasizedCard
        title="Weekly Orders"
        subText={string_of_int(siteStatistics.weeklyOrders)}
        icon=EmphasizedCard.Bookmark
        variant=EmphasizedCard.Blue
      />
    </div>
    <div className="col-md-4 stretch-card grid-margin">
      <EmphasizedCard
        title="Current Visitors Online"
        subText={string_of_int(siteStatistics.currentVisitorsOnline)}
        icon=EmphasizedCard.Diamond
        variant=EmphasizedCard.Green
      />
    </div>
  </div>
}
