module Fragment = %relay(`
  fragment Avatar_user on User {
    avatarUrl
    fullName
  }
`)

@react.component
let make = (~user as userRef) => {
  let user = Fragment.use(userRef)

  <>
    {switch user.avatarUrl {
    | Some(avatarUrl) => <img src=avatarUrl className="mr-2" alt="image" />
    | None => React.null
    }}
    {React.string(user.fullName)}
  </>
}
