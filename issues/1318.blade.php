{{-- <x-web.forms.errors --}}

@props(
       ['attr_name',
        'errors',
        'safe' => false,
       ])

@foreach ($errors->get($attr_name) as $message)
  @if($safe === true)
    <p class="error-style"><span>{!! $message !!}</span></p>
  @else
    <p class="error-style"><span>{{ $message }} </span></p>
  @endif
@endforeach
