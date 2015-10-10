<html>
  <head>
    <title>@yield('title') - BoardSoc</title>
    <link rel="stylesheet" href="{{ asset('css/app.css')}}">
    <meta charset="UTF-8">
  </head>
  <body>
    {!! Navbar::top()
              ->withBrand('BoardSoc') !!}

    <div>
      {!!
      Navbar::top() !!}
    </div>

    @section('sidebar')
      @parent

      <p>This is appended to the master sidebar.</p>
    @endsection


</html>
