<html>
  <head>
    <title>Welcome!</title>
  </head>
  <body>
    <#-- Greet the user with his/her name -->
    <h1>Welcome ${user}!</h1>
    <p>We have these animals:
      <ul>
        <#list animals as animal>
          <li>${animal.name} for ${animal.price} Euros
        </#list>
      </ul>
  </body>
</html>
