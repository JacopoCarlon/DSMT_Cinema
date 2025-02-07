<%@ page import="java.util.List" %>
<%@ page import="java.io.OutputStream" %>
<%@ page import="org.example.CinemaBooking.dto.ShowWithBookings" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
    <head>
        <title>User Private Page</title>
        <link rel="icon" type="image/x-icon" href="${pageContext.request.contextPath}/images/ScorseseAbsoluteCinema.png">
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>
        <script type="text/javascript" src="<%=request.getContextPath()%>/js/user_page_websocket.js"></script>
    </head>
    <body onload="connect('<%=request.getContextPath()%>', '<%=request.getSession().getAttribute("username")%>', '<%=request.getSession().getAttribute("is_cinema")%>' );">
        <jsp:include page="${pageContext.request.contextPath}/includes/header.jsp" />
        <div class="container">
            <div class="d-flex d-flex justify-content-between p-3">
                <a href="<%=request.getContextPath()%>/LoginServlet" class="btn btn-danger">Logout</a>
                <h4 id="h4username"> UserName: <%=request.getSession().getAttribute("username")%></h4>
                <h4 id="h4isCinema"> Is_A_Cinema: <%=request.getSession().getAttribute("is_a_cinema")%></h4>
            </div>

            <div class="card" id="current_bookings_card">
                <h3 class="d-flex justify-content-center p-3">
                    Current Bookings
                </h3>
                <div class="p-4 d-flex flex-wrap" id="parent_bookings_list">
                    <%
                    List<ShowWithBookings> showWithBookingsList = (List<ShowWithBookings>) request.getAttribute("showWithBookingsList");
                    if(showWithBookingsList == null || showWithBookingsList.size() == 0){
                    %>
                        <h5 class="d-flex justify-content-center p-3" id="noBookings">Nothing to Show<h5>
                    <%
                    } else {
                        for(int i=0; i<showWithBookingsList.size(); i++){
                            ShowWithBookings this_booking = showWithBookingsList.get(i);
                            Long this_committed_bookings = this_booking.getFirstCommittedBooking();
                    %>
                            <form class="card w-25" action="<%=request.getContextPath()%>/UserPageServlet" method="post">
                                <div class="card-body d-flex flex-column justify-content-between p-3">
                                    <div>
                                        <input type="hidden" name="showID" value="<%=this_booking.getShowID()%>">
                                        <h5 class="card-title">Prenotazione <%=i%> </h5>
                                        <div>showName: <%=this_booking.getShowName()%></div>
                                        <div>cinemaName: <%=this_booking.getCinemaName()%></div>
                                        <div>showDate: <%=this_booking.getShowDate()%></div>
                                        <div>number booked seats by you registered in server: <%=this_committed_bookings%></div>
                                    </div>
                                    <button type="submit" class="btn btn-primary m-3">Enter</button>
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
