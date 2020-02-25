<select>
  @foreach ($options as $option)
    <option
      class="foo bar"
      @if ($longComplicatedCheckThatYouDontWantInline)
        selected="selected"
      @elseif ($somethingElse)
        disabled="disabled"
      @endif
    >
      {{ $option->label }}
    </option>
  @endforeach
</select>
