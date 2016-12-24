axios.get('https://some-url')
  .then(JSON.parse)
  .then(console.log)
  .catch(console.error);
