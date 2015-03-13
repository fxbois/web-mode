<div>

  Hello, {!! $name !!}.

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
