let makeAddToStr = (container, newStr) => container := container^ ++ newStr;
let makeAddToList = (container, newMember) =>
  container := [newMember, ...container^];

/**
 * To avoid having to pattern match on module access of arbitrary
 * length (read: using dots in value of custom scalar), we mask dots
 * in custom scalar values here and then unmask them when adding the
 * type reference in Reason.
 */

[@gentype]
let maskDots = str =>
  Tablecloth.String.(str |> split(~on=".") |> join(~sep="__oo__"));

[@gentype]
let unmaskDots = str =>
  Tablecloth.String.(str |> split(~on="__oo__") |> join(~sep="."));