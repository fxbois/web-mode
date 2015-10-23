<!DOCTYPE html>
<html lang="en">
  <head>
    <title>@yield('title') - BoardSoc</title>
    <link rel="stylesheet" href="{{ asset('css/app.css')}}">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css">
    <meta charset="UTF-8">
  </head>
  <body>
    {!! Navbar::top()->withBrand('Foo') !!}

    {!! Navbar::top()
              ->withBrand('Card/BoardSoc')
              ->withContent(
                Navigation::links([
                  [
                    'title' => 'Home',
                    'link' => route('home'),
                  ],
                  [
                    'title' => 'About/Contact',
                    'link' => route('about'),
                  ],
                  [
                    'title' => 'Events',
                    'link' => route('events.index'),
                  ],
                  [
                    'title' => 'Library',
                    'link' => route('library.index'),
                  ],
                  [
                    'title' => 'Achievements',
                    'link' => route('achievements.index'),
                  ],
                  [
                    'title' => 'Admin',
                    'link' => route('admin.index'),
                    'callback' => function() {
                      return Auth::check() && Auth::user()->is_committee;
                    }
                  ],
                ])
              )
              ->withContent(
                $loginFormOrLogoutLink
              )
    !!}

    @yield('page-header')

    @if (Session::has('flash_notification.message'))
      <div class="container">
        <div class="alert alert-{{ Session::get('flash_notification.level') }}">
          <button type="button"
                  class="close"
                  data-dismiss="alert"
                  aria-hidden="true">
            &times;
          </button>

          {{ Session::get('flash_notification.message') }}
        </div>
      </div>
    @endif

    @foreach($errors->all() as $error)
      <div class="container">
        <div class="alert alert-danger">
          {{ $error }}
        </div>
      </div>
    @endforeach

    @yield('content')


    <div id="footerwrap">
      <div class="container">
        <div class="row">
          <div class="col-lg-4">
            <h4>About</h4>
            <div class="hline-w"></div>
            <p>
              BoardSoc is a student society of the University of
              Sheffield, focused on playing modern(ish) board games.
              Interested in more information? Then
              {!! link_to_route('about', 'read more here') !!}!
            </p>
          </div>
          <div class="col-lg-4">
            <h4>Social Links</h4>
            <div class="hline-w"></div>
            <p>
              <a href="https://www.facebook.com/groups/419413234788703/" title="Facebook group"><i class="fa fa-facebook"></i></a>
              <a href="mailto:boardsoc@sheffield.ac.uk"><i class="fa fa-envelope"></i></a>
            </p>
          </div>
          <div class="col-lg-4">
            <h4>Our Bunker</h4>
            <div class="hline-w"></div>
            <address>
              Sheffield Card/BoardSoc<br />
              University Of Sheffield<br />
              10-12 Brunswick Street<br />
              Sheffield, Eng S10 2FN
            </address>
          </div>

        </div><! --/row -->
      </div><! --/container -->
    </div><! --/footerwrap -->


    {!! Helpers::js() !!}
    <script src="{{ asset('search.js') }}"></script>
    <script src="{{ asset("js/retina-1.1.0.js") }}"></script>
    <script src="{{ asset("js/jquery.hoverdir.js") }}"></script>
    <script src="{{ asset("js/jquery.hoverex.min.js") }}"></script>
    <script src="{{ asset("js/jquery.prettyPhoto.js") }}"></script>
    <script src="{{ asset("js/jquery.isotope.min.js") }}"></script>
    <script src="{{ asset("js/custom.js") }}"></script>
  </body>
</html>
