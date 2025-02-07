<%@ page import="org.example.CinemaBooking.dto.Show" %>
<%@ page import="java.util.List" %>
<%@ page import="java.io.OutputStream" %>
<%@ page import="org.example.CinemaBooking.dto.Cinema" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
    <head>
        <title>Cinema Page</title>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>
        <!-- <script type="text/javascript" src="<%=request.getContextPath()%>/js/cinema_page_websocket.js"></script> -->
    </head>
    <body onload="connect('<%=request.getContextPath()%>', '<%=request.getSession().getAttribute("username")%>', '<%=request.getSession().getAttribute("is_cinema")%>' );">
        <div class="container">
            <div class="d-flex d-flex justify-content-between p-3">

                <a href="<%=request.getContextPath()%>/LoginServlet" class="btn btn-danger">Logout</a>
                <%
                Object cinemaObj = request.getAttribute("cinemaInfo");
                if (cinemaObj instanceof Cinema) {
                    Cinema cinema = (Cinema) cinemaObj;
                %>

                    <h4 id="cinemaName"> Cinema Name: <%=cinema.getCinemaName()%></h4>
                    <h4 id="cinemaLocation"> Address: <%=cinema.getCinemaLocation()%></h4>
                    <%
                    if (
                            request.getSession().getAttribute("is_a_cinema") == "true" &&
                            (Long) request.getSession().getAttribute("username") == cinema.getCinemaID()
                    ){
                    %>
                        <a href="<%=request.getContextPath()%>/CreateShowServlet" class="btn btn-primary">CreateShow for this Cinema !</a>
                    <%
                    }
                    %>
                <%
                } // end if (cinemaObj instanceof Cinema)
                else {
                %>
                    <div class="alert alert-danger" role="alert">
                        This Cinema doesn't exists!
                    </div>
                <%
                }
                %>
            </div>

            <div class="card" id="current_cinema_card">
                <h3 class="d-flex justify-content-center p-3">
                    Shows of this Cinema
                </h3>
                <div class="p-4 d-flex flex-wrap" id="parent_cinema_list">
                    <%
                    Object showsListObj = request.getAttribute("showList");
                    if(!(showsListObj instanceof List<?>) || ((List<?>)showsListObj).isEmpty()){
                    %>
                        <h5 class="d-flex justify-content-center p-3" id="noShows">Nothing to Show</h5>
                    <%
                    } else {
                        for(Object showObj : (List<?>) showsListObj){
                            if (showObj instanceof Show) {
                                Show this_show = (Show) showObj;
                    %>
                            <form class="card w-25" action="<%=request.getContextPath()%>/CinemaPageServlet" method="post">
                                <div class="card-body d-flex flex-column justify-content-between p-3">
                                    <div>
                                        <input type="hidden" name="showID" value="<%=this_show.getShowID()%>">
                                        <h5 class="card-title"> <%=this_show.getShowName()%> </h5>
                                        <div>Date: <%=this_show.getShowDate()%></div>
                                        <div>Max Seats: <%=this_show.getMaxSeats()%></div>
                                        <div>Available Seats according to main server : <%=this_show.getCurrAvailableSeats()%></div>
                                    </div>
                                    <% if (!this_show.getIsEnded()) %>
                                        <button type="submit" class="btn btn-primary m-3">Enter</button>
                                </div>
                            </form>
                    <%
                            } // end if (showObj instanceof Show this_show)
                        } // end for
                    }
                    %>
                </div>
            </div>
        </div>
    </body>
</html>
