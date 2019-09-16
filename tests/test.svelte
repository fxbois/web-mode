<svelte:head>
  <link rel="stylesheet" href="tutorial/dark-theme.css">
</svelte:head>

{#each $items as {id, name}, i}
  <div on:mousemove={handleClick}>
    {id} {"name"}
  </div>
  {@html expression}
  {@debug a, b, c}
{/each}

{#if $something}
  {#if something2}
    Sub Case
  {/if}
{:else}
  Another case
{/if}

{#await promise}
  <p>...waiting</p>
{:then number}
  <p>The number is {number}</p>
{:catch error}
  <p style="color: red">{error.message}</p>
{/await}
