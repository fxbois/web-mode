@php
$isActive = false;
$hasError = true;
@endphp

<div>
  {{ $attributes->whereDoesntStartWith('wire:model') }}
</div>

<span @class([
    'p-4',
    'font-bold' => $isActive,
    'text-gray-500' => ! $isActive,
    'bg-red' => $hasError,
])></span>

<span class="p-4 text-gray-500 bg-red"></span>
