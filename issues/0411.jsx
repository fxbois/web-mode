function hello() {

  return (
    <div>
      {somevar}
      <div className="row1">
        <div className="col-md-4">
          Column1
        </div>
        <div className="col-md-4">
          Column2
        </div>
      </div>
    </div>
  );

}

Landing = React.createClass({
  render: function() {
    return (
      <ul>
        {for(e in list){
          console.log("not aligned");
         }}
      </ul>
    );
  }
});
