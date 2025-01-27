<%@ page import="dto.Cinema" %>
<%@ page import="dto.Show" %>
<%@ page import="java.util.List" %>
<%@ page import="java.io.OutputStream" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
    <head>
        <title>Cinema Page</title>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>
        <script type="text/javascript" src="<%=request.getContextPath()%>/js/cinema_page_websocket.js"></script>
    </head>
    <body onload="connect('<%=request.getContextPath()%>', '<%=request.getSession().getAttribute("username")%>', '<%=request.getSession().getAttribute("is_cinema")%>' );">
        <div class="container">
            <div class="card" id="current_cinema_card">
                <h3 class="d-flex justify-content-center p-3">
                    Shows of this Cinema
                </h3>
                <div class="p-4 d-flex flex-wrap" id="parent_cinema_list">
                    <%
                    List<Shows> showsList = (List<Shows>) request.getAttribute("showsList");
                    if(showsList == null || showsList.size() == 0){
                    %>
                        <h5 class="d-flex justify-content-center p-3" id="noShows">Nothing to Show<h5>
                    <%
                    } else {
                        for(int i=0; i<showsList.size(); i++){
                            Show this_show = showsList.get(i);
                    %>
                            <form class="card w-25" action="<%=request.getContextPath()%>/CinemaPageServlet" method="post">
                                <div class="card-body d-flex flex-column justify-content-between p-3">
                                    <div>
                                        <input type="hidden" name="showID" value="<%=this_show.getShowID()%>">
                                        <input type="hidden" name="showName" value="<%=this_show.getShowName()%>">
                                        <input type="hidden" name="showDate" value="<%=this_show.getShowDate()%>">
                                        <input type="hidden" name="max_seats" value="<%=this_show.getMaxSeats()%>">
                                        <input type="hidden" name="curr_seats" value="<%=this_show.getCurrAvailableSeats()%>">
                                        <input type="hidden" name="is_ended" value="<%=this_show.getIsEnded()%>">
                                        <h5 class="card-title">Prenotazione <%=i%> </h5>
                                        <div>ShowID: <%=this_show.getShowID()%></div>
                                        <div>showName: <%=this_show.getShowName()%></div>
                                        <div>showDate: <%=this_show.getShowDate()%></div>
                                        <div>maxSeats: <%=this_show.getMaxSeats()%></div>
                                        <div>currAvailableSeats: <%=this_show.getCurrAvailableSeats()%></div>
                                        <div>isEnded: <%=this_show.getIsEnded()%></div>
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
