<div>
  @field([
    'label' => 'Foo',
    'values' => ['bar']
  ])
</div>

<div>
  @section('content')
    Foo
  @endsection
</div>

<div>
  @sectionInner(['name' => 'Foo'])
    <h1>foo</h1>
  @endsectionInner
</div>
