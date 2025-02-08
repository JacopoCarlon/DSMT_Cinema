
<%@ page import="java.util.List" %>
<%@ page import="org.example.CinemaBooking.dto.ShowWithBookings" %>
<%@ page import="org.example.CinemaBooking.dto.CustomerBooking" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>ERROR Page</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>

    <script type="text/javascript" src="<%=request.getContextPath()%>/js/show_page_websocket.js"></script>
</head>
<body>
<jsp:include page="../includes/header.jsp" />
<div class="container">
    <div class="d-flex d-flex justify-content-between p-3">
        <a href="<%=request.getContextPath()%>/LoginServlet" class="btn btn-danger">Logout</a>
        <h4 id="h4username"> UserName: <%=request.getSession().getAttribute("username")%></h4>
        <h4 id="h4isCinema"> Is_A_Cinema: <%=request.getSession().getAttribute("is_a_cinema")%></h4>
    </div>
    <div class="alert alert-danger" role="alert">
        <%=request.getSession().getAttribute("errorMsg")%>
    </div>
</div>
</body>
</html>
