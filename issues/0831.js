axios.get('https://some-url')
  .then(JSON.parse)
  .then(console.log)
  .catch(console.error);

// a promise returned from a function
const someFunc  = () => {
  return somePromise()
    .then()
    .catch()
}

// implicit return
const someFunc = () =>
  somePromise()
    .then() // no indent
    .catch() // no indent
}
