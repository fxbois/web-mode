<%
set objBrowserCapabilities = Server.CreateObject("MSWC.BrowserType")
bCookiesSupported = objBrowserCapabilities.cookies
session("sGotoPage") = ""
'' vsdf
''if bDebug then
''	response.write bCookiesSupported
''	response.write Session.SessionID
''	response.end
''end if
%>
<% ''^^^ End Startup Code %>

<html>
	<body>
	  <div>
		Test
	  </div>
	</body>
</html>
