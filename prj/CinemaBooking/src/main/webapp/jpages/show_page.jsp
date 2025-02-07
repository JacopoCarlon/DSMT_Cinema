
<%@ page import="java.util.List" %>
<%@ page import="org.example.CinemaBooking.dto.ShowWithBookings" %>
<%@ page import="org.example.CinemaBooking.dto.CustomerBooking" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
    <head>
        <title>Show Page</title>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>

        <script type="text/javascript" src="<%=request.getContextPath()%>/js/show_page_websocket.js"></script>
    </head>
    <%
      ShowWithBookings this_showWithBookings = (ShowWithBookings) request.getSession().getAttribute("currentSWB");
    %>
    <body onload="connect('<%=request.getContextPath()%>', '<%=request.getSession().getAttribute("username")%>', '<%=request.getSession().getAttribute("is_cinema")%>' );">
        <div class="container">
            <div class="d-flex d-flex justify-content-between p-3">
                <a href="<%=request.getContextPath()%>/LoginServlet" class="btn btn-danger">Logout</a>
                <h4 id="h4username"> UserName: <%=request.getSession().getAttribute("username")%></h4>
                <h4 id="h4isCinema"> Is_A_Cinema: <%=request.getSession().getAttribute("is_a_cinema")%></h4>
            </div>
            <div class="card" id="current_bookings_card">
                <h3 class="d-flex justify-content-center p-3">
                    this show :
                </h3>
                <div class="d-flex justify-content-center">
                    <div id="des_showName"      > showName : <%=this_showWithBookings.getShowName()%></div>
                    <div id="des_showDate"      > showDate : <%=this_showWithBookings.getShowDate()%></div>
                    <div id="des_cinemaName"     > cinemaName : <%=this_showWithBookings.getCinemaName()%></div>
                    <div id="des_cinemaLocation" > cinemaLocation : <%=this_showWithBookings.getCinemaLocation()%></div>
                    <div id="maxSeats" > maxSeats : <%=this_showWithBookings.getMaxSeats()%></div>
                    <div id="currAvailableSeats" > currAvailableSeats : <%=this_showWithBookings.getCurrAvailableSeats()%></div>
                    <div id="isEnded" > isEnded : <%=this_showWithBookings.getIsEnded()%></div>
                </div>
                <div class="p-4 d-flex flex-wrap" id="changes_form_parent">

                <%
                    String is_a_cinema = (String) request.getSession().getAttribute("is_a_cinema");

                    if("true".equals(is_a_cinema)){
                        List<CustomerBooking> committedBookingsList = this_showWithBookings.getCommittedBookingsList();
                        List<CustomerBooking> waitingBookingsList = this_showWithBookings.getWaitingForCommitList();
                        List<ShowWithBookings.Triple> tripleList = this_showWithBookings.getFullOuterJoinBookings();

                %>
                    <h4 class="d-flex justify-content-center p-3" id="comBookings">Triplet listings : </h4>

                    <table width="80%" border="1">
                        <thead>
                            <tr>
                                <td>UserName<\td>
                                <td>ConfirmedBooking<\td>
                                <td>WaitingBooking<\td>
                            <\tr>
                        <\thead>
                        <tbody>
                        <%
                            for(ShowWithBookings.Triple this_triple : tripleList){
                        %>
                            <tr>
                                <td> <%=this_triple.getUsername()%> <\td>
                                <td> <%=this_triple.getStoredBooking()%> <\td>
                                <td> <%=this_triple.getWaitingBooking()%> <\td>
                            <\tr>
                            <%
                            }
                        %>
                        <\tbody>
                    </table>

                    <h4 class="d-flex justify-content-center p-3" id="comBookings">Committed Bookings : </h4>
                <%
                        for(int i=0; i<committedBookingsList.size(); i++){
                            CustomerBooking this_booking = committedBookingsList.get(i);
                            String custName = this_booking.getCustomer();
                            Long custCommittedSeats = this_booking.getBookedSeats();
                %>
                            <div > custName : <%=custName%></div>
                            <div > custCommittedSeats : <%=custCommittedSeats%></div>
                        <%
                        }
                %>
                    <h4 class="d-flex justify-content-center p-3" id="comBookings">Waiting for Commit Bookings :</h4>
                <%
                        for(int i=0; i<waitingBookingsList.size(); i++){
                            CustomerBooking this_booking = waitingBookingsList.get(i);
                            String custName = this_booking.getCustomer();
                            Long custWaitingForCommitSeats = this_booking.getBookedSeats();
                %>
                            <div > custName : <%=custName%></div>
                            <div > custWaitingForCommitSeats : <%=custWaitingForCommitSeats%></div>
                <%
                        }
                    } else {
                        Long your_committed_booking = this_showWithBookings.getFirstCommittedBooking();
                        Long your_waiting_booking = this_showWithBookings.getFirstWaitingBooking();
                %>
                    <div class="d-flex justify-content-center">
                        <div id="des_committed_booking"        > your_committed_booking : <%=your_committed_booking%></div>
                        <div id="des_waiting_booking"      > your_waiting_booking : <%=your_waiting_booking%></div>
                    </div>
                    <form action="<%=request.getContextPath()%>/ShowPageServlet" method="post" oninput='check_valid_booking()'>
                        <div class="d-flex justify-content-between mb-3">
                            <div class="mb-3">
                                <label for="new_booking_number" class="form-label">Enter your new_booking_number</label>
                                <input type="number"
                                    class="form-control"
                                    name="new_booking_number"
                                    id="new_booking_number"
                                    min="0"
                                    max="144000000"
                                    aria-describedby="bid"
                                    required>
                            </div>
                            <div>
                                <button type="submit" class="btn btn-primary mx-2 px-4"> Set this as new booking </button>
                            </div>
                        </div>
                    </form>
                <%
                    }
                %>
                </div>
            </div>
        </div>
    </body>
</html>
