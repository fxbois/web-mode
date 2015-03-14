<div>

  Hello, {!! $name !!}.

  {{ isset($name) ? $name : 'Default' }}

  @{{ This will not be processed by Blade }}

  {{-- This comment will not be in the rendered HTML --}}

  @if (count($records) === 1)
    I have one record!
  @elseif (count($records) > 1)
    I have multiple records!
  @else
    I don't have any records!
  @endif

  @unless (Auth::check())
    You are not signed in.
  @endunless

  Anothter example:

  @section('box-content')

    <h4>Item </h4>
    {!!Form::open( ['route' => ['menu.store'] , 'id' => 'form'])!!}

    @section('list.item.content')
      <p>This is an item of type {{$item->type}}</p>
    @overwrite

    <div>
      @section('sidebar')
        This is themaster sidebar.
      @show
      <div class="container">
        @yield('content')
      </div>
    </div>

  @show

</div>
