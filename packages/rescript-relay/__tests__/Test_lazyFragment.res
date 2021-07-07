module Fragment = %relay(
  `
    fragment TestLazyFragment_user on User @refetchable(queryName: "TestLazyFragmentQuery") {
      firstName
      lastName
    }
`
)

module Test = {
  @react.component
  let make = () => {
    let data = Fragment.useLazy(~variables={id: "user-1"}, ())

    switch data {
    | Some(user) => <div> {React.string("Name: " ++ user.firstName ++ " " ++ user.lastName)} </div>
    | None => React.null
    }
  }
}

let test_lazyFragment = () => {
  let network = RescriptRelay.Network.makePromiseBased(~fetchFunction=RelayEnv.fetchQuery, ())

  let environment = RescriptRelay.Environment.make(
    ~network,
    ~store=RescriptRelay.Store.make(~source=RescriptRelay.RecordSource.make(), ()),
    (),
  )
  ()

  <TestProviders.Wrapper environment> <Test /> </TestProviders.Wrapper>
}
