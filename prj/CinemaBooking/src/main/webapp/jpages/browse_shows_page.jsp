<%@ page import="org.example.CinemaBooking.dto.Show" %>
<%@ page import="java.util.List" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>Available Shows Page</title>
    <link rel="icon" type="image/x-icon" href="${pageContext.request.contextPath}/images/ScorseseAbsoluteCinema.png">
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>
    <script type="text/javascript" src="<%=request.getContextPath()%>/js/browse_shows_websocket.js"></script>
</head>
<body onload="connect('<%=request.getContextPath()%>', '<%=request.getSession().getAttribute("username")%>', '<%=request.getSession().getAttribute("is_cinema")%>' );">
    <jsp:include page="../includes/header.jsp" />
    <div class="container">
        <div class="d-flex d-flex justify-content-between p-3">
            <a href="<%=request.getContextPath()%>/LoginServlet" class="btn btn-danger">Logout</a>
            <h4 id="h4username"> Username: <%=request.getSession().getAttribute("username")%></h4>
            <h4 id="h4isCinema"> Is_A_Cinema: <%=request.getSession().getAttribute("is_a_cinema")%></h4>
        </div>

        <div class="card" id="list_of_shows_card">
            <h3 class="d-flex justify-content-center p-3">
                List of Shows
            </h3>
            <div class="p-4 d-flex flex-wrap" id="parent_show_list">
                <%
                    Object showListObject = request.getAttribute("allShowsList");
                    List<Show> showsList = null;
                    // safely cast list of shows
                    if (showListObject instanceof List<?> objList) {
                        showsList = objList.stream().filter(obj -> obj instanceof Show).map(obj -> (Show) obj).toList();
                    }
                    if(showsList == null || showsList.isEmpty()){
                %>
                        <h5 class="d-flex justify-content-center p-3" id="noShows">Nothing to Show</h5>
                <%
                    } else {
                        for(Show this_show : showsList){
                %>
                        <form class="card w-25" action="<%=request.getContextPath()%>/BrowseShowsServlet" method="post">
                            <div class="card-body d-flex flex-column justify-content-between p-3">
                                <div>
                                    <input type="hidden" name="showID" value="<%=this_show.getShowID()%>">
                                    <h5 class="card-title"><%=this_show.getShowName()%></h5>
                                    <div>showDate: <%=this_show.getShowDate()%></div>
                                    <div>Cinema: <%=this_show.getCinemaName()%>, in: <%=this_show.getCinemaLocation()%></div>
                                    <div>Available Seats: <%=this_show.getCurrAvailableSeats()%>/<%=this_show.getMaxSeats()%></div>
                                </div>
                                <button type="submit" class="btn btn-primary m-3">Go to Show page</button>
                            </div>
                        </form>
                <%
                        }
                    }
                %>
            </div>
        </div>
    </div>
</body>
</html>
