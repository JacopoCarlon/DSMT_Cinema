<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
    <head>
        <title>Create Show</title>
        <link rel="icon" type="image/x-icon" href="${pageContext.request.contextPath}/images/ScorseseAbsoluteCinema.png">
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>
    </head>
    <body>
        <jsp:include page="../includes/header.jsp" />
        <div class="container">
            <%
            if (request.getSession().getAttribute("is_a_cinema") == "true") {
                Long cinemaID = (Long) request.getSession().getAttribute("username");
            %>
                <div class="d-flex p-3">
                    <a href="<%=request.getContextPath()%>/CinemaPageServlet?cinemaID=<%=cinemaID%>" class="btn btn-danger">Back</a>
                </div>
                <div class="card">
                    <h3 class="d-flex justify-content-center p-3">
                        Create a new Show !!!
                    </h3>
                    <div class="card-body">
                        <form action="<%=request.getContextPath()%>/CreateShowServlet" method="post" id="create_show_form">
                            <div class="mb-3">
                                <label for="showName" class="form-label">Show Name</label>
                                <input type="text" class="form-control" name="showName" placeholder="Enter showName" aria-describedby="showName" id="showName" required>
                            </div>
                            <div class="mb-e">
                                <label for="showDate">Show Date and Time:</label>
                                <input type="datetime-local" id="showDate" name="showDate" value="2025-11-11T11:11" min="2025-01-01T00:01" max="2150-12-31T11:59" required/>
                            </div>
                            <div class="mb-3">
                                <label for="maxSeats" class="form-label">Maximum Seats</label>
                                <div class="input-group">
                                    <input type="number" class="form-control" name="maxSeats" min="0" max="144000000"  id="maxSeats" required>
                                    <div class="input-group-append">
                                        <span class="input-group-text">s</span>
                                    </div>
                                </div>
                            </div>
                            <%
                                String registrationStatus = (String) request.getSession().getAttribute("showCreationStatus");
                                if(registrationStatus != null && registrationStatus.equals("error")) {
                            %>
                            <div class="alert alert-danger" role="alert">
                                Something went wrong during Show creation, please retry later.
                            </div>
                            <%
                            }else if(registrationStatus != null && registrationStatus.equals("success")){
                            %>
                            <div class="alert alert-danger" role="alert">
                                The show has been properly created!!!
                            </div>
                            <%
                                }
                            %>
                            <button type="submit" class="btn btn-primary m-3">Create Show !!! </button>
                        </form>
                    </div>
                </div>
            <%
            } // end if (request.getSession().getAttribute("is_a_cinema") == "true")
            else {
            %>
                <div class="alert alert-danger" role="alert">
                    You shouldn't be here!
                </div>
            <%
            }
            %>
        </div>
    </body>
</html>
