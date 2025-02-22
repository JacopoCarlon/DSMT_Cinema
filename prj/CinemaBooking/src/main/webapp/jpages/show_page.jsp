
<%@ page import="java.util.List" %>
<%@ page import="org.example.CinemaBooking.dto.ShowWithBookings" %>
<%@ page import="org.example.CinemaBooking.dto.CustomerBooking" %>
<%@ page import="com.google.gson.Gson" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
    <head>
        <title>Show Page</title>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>

        <link href="${pageContext.request.contextPath}/css/style.css">
        <link href="<%=request.getContextPath()%>/css/style.css">
        <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/style.css">

        <script type="text/javascript" src="<%=request.getContextPath()%>/js/show_page_websocket.js"></script>
    </head>
    <%
      ShowWithBookings this_showWithBookings = (ShowWithBookings) request.getSession().getAttribute("currentSWB");
    %>
    <body onload="connect(
                    '<%=request.getContextPath()%>',
                    '<%=this_showWithBookings.getShowID()%>',
                    <%=request.getSession().getAttribute("is_a_cinema")%>,
                    '<%=request.getSession().getAttribute("username")%>'
    );">
        <jsp:include page="../includes/header.jsp" />
        <div class="container">
            <div class="d-flex d-flex justify-content-between p-3">
                <h4 id="h4username"> UserName: <%=request.getSession().getAttribute("username")%></h4>
                <h4 id="h4isCinema"> Is_A_Cinema: <%=request.getSession().getAttribute("is_a_cinema")%></h4>
            </div>
            <div class="card" id="current_show_card">
                <h3 class="d-flex justify-content-center p-3">
                    <%=this_showWithBookings.getShowName()%>
                </h3>
                <div class="p-4 d-flex flex-column justify-content-center">
                    <div>When:     <%=this_showWithBookings.getShowDate()%></div>
                    <div>
                        Where:
                        <a href="<%=request.getContextPath()%>/CinemaPageServlet?cinemaID=<%=this_showWithBookings.getCinemaID()%>">
                            <%=this_showWithBookings.getCinemaName()%>
                        </a>
                         in <%=this_showWithBookings.getCinemaLocation()%>
                    </div>
                    <div>
                        Available Seats:
                        <span id="des_currAvailableSeats"><%=this_showWithBookings.getCurrAvailableSeats()%></span>/
                        <span id="des_maxSeats"><%=this_showWithBookings.getMaxSeats()%></span>
                    </div>
                    <br>
                </div>
                <div class="p-4 d-flex flex-column">
                <%
                    String is_a_cinema = (String) request.getSession().getAttribute("is_a_cinema");
                    if("true".equals(is_a_cinema)){
                        // page requested from a Cinema
                        List<ShowWithBookings.Triple> tripleList = this_showWithBookings.getFullOuterJoinBookings();
                %>
                    <table>
                        <thead>
                            <tr>
                                <th>Customer</th>
                                <th>Booking stored in Database</th>
                                <th>Booking stored in Show node</th>
                            </tr>
                        </thead>
                        <tbody>
                        <%
                            for(ShowWithBookings.Triple this_triple : tripleList){
                        %>
                            <tr>
                                <td> <%=this_triple.getUsername()%> </td>
                                <td> <%=this_triple.getStoredBooking()%> </td>
                                <td> <%=this_triple.getWaitingBooking()%> </td>
                            </tr>
                            <%
                            }
                        %>
                        </tbody>
                    </table>
                <%
                    } // end if("true".equals(is_a_cinema))
                    else {
                        // page requested from a Customer
                        Long your_committed_booking = this_showWithBookings.getFirstCommittedBooking();
                        Long your_waiting_booking = this_showWithBookings.getFirstWaitingBooking();
                        boolean waiting_commit = (your_waiting_booking != null);
                %>
                    <div class="d-flex flex-column justify-content-center">
                        <div> Committed Bookings: <span id="des_committed_booking"><%=your_committed_booking%></span></div>
                        <div <% if (!waiting_commit) {%> style="display: none;" <%}%> > New value waiting to be committed:
                            <span id="des_waiting_booking"><%=waiting_commit ? your_waiting_booking : your_committed_booking%></span>
                        </div>
                        <br>
                    </div>
                    <% if (!this_showWithBookings.getIsEnded()) {%>
                        <form action="<%=request.getContextPath()%>/ShowPageServlet" method="post" onsubmit='check_valid_booking()'>
                            <div class="d-flex flex-column justify-content-between mb-3">
                                <div class="mb-3">
                                    <label for="new_booking_number" class="form-label">Enter your new_booking_number</label>
                                    <input type="number"
                                        class="form-control"
                                        name="new_booking_number"
                                        id="new_booking_number"
                                        aria-describedby="new_booking_number"
                                        placeholder="0"
                                        min="0"
                                        max="144000000"
                                        required>
                                </div>
                                <div>
                                    <button type="submit" class="btn btn-primary mx-2 px-4"> Set this as new booking </button>
                                </div>
                                <%
                                    String newBookingRequestStatus = (String) request.getSession().getAttribute("bookingUpdateStatus");
                                    if(newBookingRequestStatus != null && newBookingRequestStatus.equals("error")) {
                                %>
                                        <div id="bookingRequestAlert" class="alert alert-danger" role="alert">
                                            Something went wrong. Retry later.
                                        </div>
                                <%
                                    }
                                %>
                            </div>
                        </form>
                <%
                    } // end if (!this_showWithBookings.getIsEnded())
                } // end else
                %>
                </div>
            </div>
        </div>
    </body>
</html>
