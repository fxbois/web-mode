router.get('/alerts', (request, response) => {
  const query = request.query;
});

router.get('/alerts', function(request, response) {
  const query = request.query;
});

// Basic syntax:
(param1, param2, paramN) => { statements }
(param1, param2, paramN) => expression
// equivalent to:  => { return expression; }

// Parentheses are optional when there's only one argument:
(singleParam) => { statements }
singleParam => { statements }

// A function with no arguments requires parentheses:
() => { statements }

// Advanced:
// Parenthesize the body to return an object literal expression:
params => ({foo: bar})

  // Rest parameters are supported
  (param1, param2, ...rest) => { statements }
