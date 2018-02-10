const a = ["I'm", "foo"];

switch (true) {
  case /I'm foo/.test(a.join(' ')):
    console.log(true);
    break;
}
