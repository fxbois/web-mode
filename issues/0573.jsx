function f() {
  var x = true ? <div></div> : '';
  return x;  // pressing tab doesn't indent.
}

function f2() {
  // pressing tab here indents to the opening brace. :(
}
