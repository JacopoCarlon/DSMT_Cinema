<html>
    <head>
        <title>Cinema Login Page</title>
        <link rel="icon" type="image/x-icon" href="${pageContext.request.contextPath}/images/ScorseseAbsoluteCinema.png">
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>

        <link href="${pageContext.request.contextPath}/css/style.css">
        <link href="<%=request.getContextPath()%>/css/style.css">
        <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/style.css">

    </head>
    <body>
        <div class="container">
            <h1 class="d-flex justify-content-center my-5">CinemaBooking - Cinema Login Page</h1>
            <div class="card">
                <div class="card-body">
                    <form method="POST" action="<%=request.getContextPath()%>/CinemaLoginServlet">
                        <div class="mb-3">
                            <label for="username" class="form-label">Cinema ID</label>
                            <input type="text" class="form-control" name="username" id="username" placeholder="12" pattern="[0-9]+" required>
                        </div>
                        <div class="mb-3">
                            <label for="password" class="form-label">Password</label>
                            <input type="text" class="form-control" name="password" id="password" placeholder="EnterPassword.1234" required>
                        </div>
                        <%
                            String loginStatus = (String) request.getSession().getAttribute("loginStatus");
                            if(loginStatus != null && loginStatus.equals("error")) {
                        %>
                            <div class="alert alert-danger" role="alert">
                                Failed login. Wrong ID or password.
                            </div>
                        <%
                            }
                        %>
                        <input value="MAGIC" type="hidden">
                        <button type="submit" class="btn btn-primary">
                            Login
                        </button>
                    </form>
                    <div>
                        <a href="<%=request.getContextPath()%>/LoginServlet">
                            Login as a Customer
                        </a>
                    </div>
                    <div>
                        Not Registered? Sign up
                        <a href="<%=request.getContextPath()%>/CinemaRegistrationServlet">
                            here
                        </a>
                    </div>
                </div>
            </div>
            <div id="img_cinema">
                <img src="${pageContext.request.contextPath}/images/ScorseseCinema.png" alt="Scorsese : cinema" >
            </div>
        </div>
    </body>
</html>
