<div>Name
  [#list animals as animal]
    <span>
      [#if animal.size == "large"]test1[/#if]
      ${animal.name}
      [#if animal.size == "large"]test2[/#if]
      ${animal.price} Euros
    </span>
  [/#list]
</div>
