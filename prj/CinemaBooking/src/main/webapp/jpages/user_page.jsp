<%@ page import="dto.Cinema" %>
<%@ page import="dto.Show" %>
<%@ page import="java.util.List" %>
<%@ page import="java.io.OutputStream" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
    <head>
        <title>User Private Page</title>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>
        <script type="text/javascript" src="<%=request.getContextPath()%>/js/user_page_websocket.js"></script>
    </head>
    <body onload="connect('<%=request.getContextPath()%>', '<%=request.getSession().getAttribute("username")%>', '<%=request.getSession().getAttribute("is_cinema")%>' );">
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
                    List<Bookings> bookingsList = (List<Bookings>) request.getAttribute("bookingList");
                    if(bookingsList == null || bookingsList.size() == 0){
                    %>
                        <h5 class="d-flex justify-content-center p-3" id="noBookings">Nothing to Show<h5>
                    <%
                    } else {
                        for(int i=0; i<bookingsList.size(); i++){
                            Booking this_booking = bookingsList.get(i);
                    %>
                            <form class="card w-25" action="<%=request.getContextPath()%>/UserPageServlet" method="post">
                                <div class="card-body d-flex flex-column justify-content-between p-3">
                                    <div>
                                        <input type="hidden" name="username" value="<%=this_booking.getUsername()%>">
                                        <input type="hidden" name="showID" value="<%=this_booking.getShowID()%>">
                                        <input type="hidden" name="showName" value="<%=this_booking.showName()%>">
                                        <input type="hidden" name="cinemaName" value="<%=this_booking.cinemaName()%>">
                                        <input type="hidden" name="showDate" value="<%=this_booking.showDate()%>">
                                        <input type="hidden" name="num_seats" value="<%=this_booking.num_seats()%>">
                                        <h5 class="card-title">Prenotazione <%=i%> </h5>
                                        <div>showName: <%=this_booking.showName()%></div>
                                        <div>cinemaName: <%=this_booking.cinemaName()%></div>
                                        <div>showDate: <%=this_booking.showDate()%></div>
                                        <div>number booked seats by you: <%=this_booking.num_seats()%></div>
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
