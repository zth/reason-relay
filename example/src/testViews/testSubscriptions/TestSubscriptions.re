module Subscription = [%relay.subscription
  {|
  subscription TestSubscriptionsSubscription {
    bookAdded {
      id
      title
      author
    }
  }
|}
];

[@react.component]
let make = () => {
  let (booksAddedCount, setBooksAddedCount) = React.useState(() => 0);
  let environment = ReasonRelay.useEnvironmentFromContext();

  React.useEffect1(
    () => {
      let disposable =
        Subscription.subscribe(
          ~environment,
          ~variables=(),
          ~onNext=
            res => {
              Js.log(res);
              setBooksAddedCount(prevCount => prevCount + 1);
            },
          (),
        );

      Some(() => disposable->ReasonRelay.Disposable.dispose);
    },
    [||],
  );

  <p> {React.string(string_of_int(booksAddedCount) ++ " books added.")} </p>;
};
