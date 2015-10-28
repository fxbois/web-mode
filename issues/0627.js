class Test {
  constructor() {
    super(...arguments);
    let a = 1 // This should not indent.
  }
}


const a = {}
const b = {
  ...a, // This should indent.
  ...b,
  ...c
}

newB.firstMethod('x')
  .secondMethod('y')
  .thirdMethod(1,15,'z');
