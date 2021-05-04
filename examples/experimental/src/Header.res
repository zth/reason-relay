@react.component
let make = () =>
  <div className="page-header">
    <h3 className="page-title">
      <span className="page-title-icon bg-gradient-primary text-white mr-2">
        <i className="mdi mdi-home" />
      </span>
      {React.string("Dashboard")}
    </h3>
  </div>
