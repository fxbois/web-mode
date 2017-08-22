<html>
  <head>
    <title>Welcome!</title>
  </head>
  <body>
    <#-- Greet the user with his/her name -->
    <h1>Welcome ${user}!</h1>
    <div>We have these animals:
      <ul>
        <#list animals as animal>
          <li>${animal.name} for ${animal.price} Euros</li>
        </#list>
      </ul>
    </div>
    [#include "/WEB-INF/common/header.ftl"]
    [#assign testVar=false]
    [#if object?has_content]
      <span class="">Has content</span>
    [#else]
      <span class="">No content</span>
    [/#if]
  </body>
</html>
