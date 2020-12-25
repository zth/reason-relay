type cardVariant =
  | Red
  | Blue
  | Green

type icon =
  | ChartLine
  | Bookmark
  | Diamond

let mapIcon = icon =>
  switch icon {
  | ChartLine => "mdi-chart-line"
  | Bookmark => "mdi-bookmark-outline"
  | Diamond => "mdi-diamond"
  }

@react.component
let make = (~title, ~subText, ~icon, ~variant) => {
  let variantAsBgClass = switch variant {
  | Red => "bg-gradient-danger"
  | Blue => "bg-gradient-info"
  | Green => "bg-gradient-success"
  }

  <div className={"card card-img-holder text-white " ++ variantAsBgClass}>
    <div className="card-body">
      <img src="/images/dashboard/circle.svg" className="card-img-absolute" alt="circle-image" />
      <h4 className="font-weight-normal mb-3">
        {React.string(title)} <i className={"mdi mdi-24px float-right " ++ mapIcon(icon)} />
      </h4>
      <h2 className="mb-5"> {React.string(subText)} </h2>
    </div>
  </div>
}
