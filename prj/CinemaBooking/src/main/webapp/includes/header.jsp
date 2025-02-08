<%--
  Created by IntelliJ IDEA.
  User: jacopo
  Date: 07/02/25
  Time: 19:55
  To change this template use File | Settings | File Templates.
--%>
<!-- includes/header.jsp -->
<div style="background-color: #f0f0f0; padding: 10px; text-align: center;">

  <!-- Button 1: Redirect based on session variable -->
  <button onclick="window.location.href='${pageContext.request.contextPath}/<%=
        "true".equals(session.getAttribute("is_a_cinema")) ?
            "CinemaPageServlet?cinemaID=" + session.getAttribute("username") :
            "UserPageServlet"
    %>'">
    Go to your private <%= "true".equals(session.getAttribute("is_a_cinema")) ? "Cinema" : "User" %> page !!!
  </button>

  <!-- Button 2: Always redirect to my_browser.jsp -->
  <button onclick="window.location.href='${pageContext.request.contextPath}/BrowseShowsServlet'">
    Go to My Browser
  </button>
</div>