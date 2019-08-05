/** Test utils **/
let makeEnvironmentWithSubscription = () => {
  let controller = ref(None);

  (
    ReasonRelay.Environment.make(
      ~network=
        ReasonRelay.Network.makePromiseBased(
          ~fetchFunction=RelayEnv.fetchQuery,
          ~subscriptionFunction=
            _ =>
              ReasonRelay.Observable.make(sink => {
                controller := Some(sink);
                ();
              }),
          (),
        ),
      ~store=ReasonRelay.Store.make(ReasonRelay.RecordSource.make()),
      (),
    ),
    controller,
  );
};
