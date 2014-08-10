if (true) {
  var x;
  /*
   * Hello there.
   * Today is Saturday.
   */
  var z;
  /* lorem
     ipsum */
  var y;
}

var pathData = this.barData
                   .map(function(d) {
                     return [{x: d.start_time_ms, vol: d.volume},
                             {x: d.end_time_ms, vol: d.volume}];
                   })
                   .reduce(function(a, b) {
                     return a.concat(b);
                   });

switch(elem) {
  case "textarea" :
    return ( <textarea rows="4" /> );
    break;
  case "text" :
    return ( <input type="text" /> );
  default :
    return null;
}

function hello() {
  if (x)
    console.log("lorem");
  else
    console.log("ipsum");
}
