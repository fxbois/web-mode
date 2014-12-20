
<html>
  <c:if>
    <p></p>
  </c:if>
</html>

<span>ss</span>

<div>
  <% if (x) { %>
    <span><%= x %></span>
  <% } else { %>
    <span><%= x %></span>
  <% } %>
</div>

<c:set var="ageTotal" value="${0}" />
<c:forEach var="person"
           items="${personList}">
  <c:set var="ageTotal" value="${ageTotal + person.age}" />
  <tr><td>${person.name}</td><td>${person.age}</td></tr>
</c:forEach>
${ageTotal}

<%@ taglib prefix="f" uri="/your-tld-uri"%>
...
<c:out value="${f:sum(personList)}"/>

<%@ taglib uri="/you-taglib-uri" prefix="p" %>
and use the tag:

<c:forEach var="person" items="${personList}">
  <tr><td>${person.name}</td><td>${person.age}</td></tr>
</c:forEach>
<p:personSum personList="${personList}"/>

<%= toto %>

<p>test</p>

<span>
  <%
  if ( haveError ) {
    request.setAttribute( "error
xss",
      //todo : erreur d'indentation
      errors.toString());
    pageContext.forward( "GetName.jsp" );
  } else {
    pageContext.forward( "GetName.jsp" );
    pageContext.forward( "NextPage.jspx" );
  }
  %>
</span>

<div>
  <%@ page import="business.*, data.*" /*comment*/
           test="*indent attr*" %>
</div>

<div>cdc</div>

<%
// This is a scriptlet.  Notice that the "date"
// variable we declare here is available in the
// embedded expression later on.
int i = 0
System.out.println( "Evaluating date now" );
java.util.Date date = new java.util.Date();
%>

<span>
  <%!
  // declare here
  public void add(User user, String filename) throws IOException, ServletException {
    PrintWriter out = new PrintWriter(new FileWriter(filename, true));
    out.println("toto" + "titi" +
      "titi" +
      user.getLastName());
    out.close();
  }

  %>
</span>

<%-- cdcd --%>
