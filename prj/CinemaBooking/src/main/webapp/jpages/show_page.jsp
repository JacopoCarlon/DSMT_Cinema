<%@ page import="dto.Booking" %>
<%@ page import="dto.BookingList" %>
<%@ page import="dto.Cinema" %>
<%@ page import="dto.Customer" %>
<%@ page import="dto.Show" %>
<%@ page import="dto.ShowExpanded" %>
<%@ page import="dto.ShowList" %>
<%@ page import="java.util.List" %>
<%@ page import="java.io.OutputStream" %>
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
      ShowExpanded this_show_expanded = (ShowExpanded) request.getSession().getAttribute("currentShowExpanded");
      String curr_username = this_show_expanded.getUsername();
      String curr_is_a_cinema = this_show_expanded.getIs_a_cinema();
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

                <h5 class="d-flex justify-content-center">
                    <div id="des_showID"        > showID : <%=this_show_expanded.getShowID()%></div>
                    <div id="des_showName"      > showName : <%=this_show_expanded.getShowName()%></div>
                    <div id="des_showDate"      > showDate : <%=this_show_expanded.getShowDate()%></div>
                    <div id="des_maxSeats"      > maxSeats : <%=this_show_expanded.getMaxSeats()%></div>
                    <div id="des_currAvailableSeats"  > currAvailableSeats : <%=this_show_expanded.getCurrAvailableSeats()%></div>
                    <div id="des_isEnded"       > isEnded : <%=this_show_expanded.getEnded()%></div>
                    <div id="des_cinemaName"    > isEnded : <%=this_show_expanded.getCinemaName()%></div>
                    <div id="des_cinemaLocation"    > isEnded : <%=this_show_expanded.getCinemaLocation()%></div>
                    <div id="des_username"      > username : <%=this_show_expanded.getUsername()%></div>
                    <div id="des_num_seats"     > num_seats : <%=this_show_expanded.getNum_seats()%></div>
                    <div id="des_is_a_cinema"   > is_a_cinema : <%=this_show_expanded.getIs_a_cinema()%></div>
                    <div id="des_testUN0"   > this_show_expanded.getUsername() : <%=this_show_expanded.getUsername()%></div>
                    <div id="des_testUN1"   > request.getSession().getAttribute(username) : <%=request.getSession().getAttribute("username")%></div>
                </h5>

                <div class="p-4 d-flex flex-wrap" id="changes_form_parent">
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
                        </div>
                            <button type="submit" class="btn btn-primary mx-2 px-4" <%=(this_show_expanded.getUsername().equals(request.getSession().getAttribute("username"))) ? "disabled" : ""%>> Set this as new booking </button>
                        </div>
                    </form>
                </div>


            </div>
        </div>
    </body>
</html>
