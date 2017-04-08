while (x) {
  if (true) a()
  b()
  c()
}

let newList = list.skipWhile(f)
a()
b()

Relay.createContainer(Story, {
  initialVariables: {
    numCommentsToShow: 10,
    showComments: false,
  },
  fragments: {
    story: (variables) => Relay.QL`
      fragment on Story {
        comments(first: $numCommentsToShow) @include(if: $showComments) {
          edges {
            node {
              author { name },
              id,
              text,
            },
          },
        },
      }
    `,
  }
});

var cs = {completed: this.props.todo.completed,
          editing: this.props.editing};

const f = (x) =>
  x + 1;

if (a)
  if (b)
    if (c)
      console.log("ici");
console.log("la");

var data = {
  password: this.password
}

VEOrganizer.prototype.unpatchVeToolbar = function() {
  ve.ui.Toolbar.prototype.getElementWindow =
    this.veUiToolbarPrototypeGetElementWindow;
  ve.ui.Toolbar.prototype.calculateOffset =
    this.veUiToolbarPrototypeCalculateOffset;
};

if(!rstart.isCollapsed() ||
   !this.ve.document.hasSlugAtOffset(rstart.to)) {
  groups = this.ve.document.getCoveredSiblingGroups(rstart);
};

if(!this.begin(ed.isNew() ? "savenewmodule" : "savemodule",
               ed.getSaveData(this.state.segmentId, values))) {
  return false;
}

windowOffset.right = (
  $(window).width()
  - this.$window.outerWidth()
  - windowOffset.left
);

if(!rstart.isCollapsed()
   || this.ve.document.data.isContentOffset(rstart.to)
   || !this.ve.document.hasSlugAtOffset(rstart.to)) {
  groups = this.ve.document.getCoveredSiblingGroups(rstart);
}

var json = {
  msg: 'hello',
  attr: condition
      ? "This message"
      : "Some other message"
}

for (let type in types)
  doSomething(type);

for (let type of types)
  doSomething(type); // should be indented

ajax.post('/signup')
    .send(data);

$(document.body)
  .append("<span/>")
  .attr(...);

$('<div style="background: #fff">Open</div>')
  .attr("data-id", "foo");

$(document.body).append("<span/>")
                .attr(...);

var str = "toto";

'test/1'.match(/[a-z]+[0-9]+/g);

'test/1'.match(/[a-z]+\/[0-9]+/ug);

return {
  nowShowing: Const.ALL_TODOS,
  editing: null
};

class Person {
  @memoize
  get name() { return `${this.first} ici ${this.last}` }
  set name(val) {
    let [first, last] = val.split(' ');
    this.first = first;
    this.last = last;
  }
}


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

var x = y,
    cd = 3;
if (true) {
  var x = y,
      cd = 3;
  export const {x, y} = z,
               a = 10;
}

var pathData = this.barData
                   .map(function(d) {
                     return [{x: d.start_time_ms, vol: d.volume},
                             {x: d.end_time_ms, vol: d.volume}];
                   })
                   .reduce(function(a, b) {
                     return a.concat(b);
                   });

function toto(c) {
  switch(elem) {
    case "textarea" :
      return ( 1 );
      break;
    case "text" :
      return ( 2 );
    default :
      return 0;
  }
}

function hello() {
  if (x)
    console.log("lorem");
  else
    console.log("ipsum");
}

var obj = {
  "a": 1,
  "b": 2,
  "c": 3
};

var arr = [1,
           2,
           3
];

function onYouTubePlayerAPIReady(videoid) {
  ytPlayer = new YT.Player('media_area',
                           {videoId: videoid,
                            playerVars: { 'rel': 0 }
                           });
}

var newB = new example();

newB.firstMethod('x')
    .secondMethod('y')
    .thirdMethod(1,15,'z');

var bar = 1;
var foo = (bar == 1) ?
          "ONE" : "OTHER";  // THIS IS THE PROBLEM

foo() {
  bar()
  baz()
  if(true)
    hello();
  if (true)
    hello();
}

function fieldIsValid( field, fieldSpec) {
  // todo: add remote possibility
  var value = field.value;
  if ( value.length < fieldSpec.minNumberOfChar ||
       value.length > fieldSpec.maxNumberOfChar ||
       value.match( fieldSpec.pattern) )
    return true;
  return false;
}
