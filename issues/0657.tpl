<div>
  {if $condition}
    <span>this aligned to {</span>
  {else}
    <span>so do this</span>
  {/if}

  {foreach $items as $i}
    <span>{$i}</span>
  {/foreach}

</div>
