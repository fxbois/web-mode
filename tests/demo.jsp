
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

<template:addResources>
  <script>
   angular.module('jahiaProperties', []).constant('JAHIA_PROPERTIES', {
     urlWsRecherche: "${currentNode.properties['webserviceRechercheOffre'].string}",
     urlWsDetail: "${currentNode.properties['webserviceDetailOffre'].string}",
     urlWsPanier: "${currentNode.properties['webservicePanierOffre'].string}",
     nbResultatsParPage: ${currentNode.properties['nbResultatsParPage'].string},
     nbRecherchesSauvegardees: ${currentNode.properties['nbRecherchesSauvegardees'].string},
     referencePageRechercheAvancee: "<c:url value='${url.base}${currentNode.properties.referencePageRechercheAvancee.node.path}.html'/>",
     referencePageListeOffres: "<c:url value='${url.base}${currentNode.properties.referencePageListeOffres.node.path}.html'/>",
     referencePageDetailOffre: "<c:url value='${url.base}${currentNode.properties.referencePageDetailOffre.node.path}.html'/>",
     referencePageGeolocalisation: "<c:url value='${url.base}${currentNode.properties.referencePageGeolocalisation.node.path}.html'/>",
     referencePageGeolocalisationDetail: "<c:url value='${url.base}${currentNode.properties.referencePageGeolocalisationDetail.node.path}.html'/>",
     referencePagePanierOffres: "<c:url value='${url.base}${currentNode.properties.referencePagePanierOffres.node.path}.html'/>",
     siteName: "${fn:toLowerCase(renderContext.site.name)}",
     rechercheCodes: ${rechercheCodes},
     rechercheAvanceeCodes: ${rechercheAvanceeCodes}
   });
  </script>
</template:addResources>


<%-- cdcd --%>
