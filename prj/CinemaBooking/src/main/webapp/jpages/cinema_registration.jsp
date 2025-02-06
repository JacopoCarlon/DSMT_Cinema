<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
    <head>
        <title>Cinema Registration Page</title>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>
    </head>
    <body>
        <div class="container">
          <h1 class="justify-content-center d-flex">CinemaBooking - Cinema Registration Page</h1>
          <div class="card">
            <div class="card-body">
              <form method="POST" action="<%=request.getContextPath()%>/CinemaRegistrationServlet"
                oninput='confirmPassword.setCustomValidity(confirmPassword.value !== password.value ? "Passwords do not match." : "")'>
                <div class="mb-3">
                    <label for="cinemaName" class="form-label">Name</label>
                    <input type="text" class="form-control" name="cinemaName" id="cinemaName" placeholder="Odeon" pattern="^(?=.{3,20}$)(?![_.])(?!.*[_.]{2})[a-zA-Z0-9._]+(?<![_.])$" required>
                </div>
                <div class="mb-3">
                    <label for="password" class="form-label">Password</label>
                    <input type="text" class="form-control" name="password" id="password" placeholder="EnterPassword.1234" required>
                </div>
                <div class="mb-3">
                    <label for="confirmPassword" class="form-label">Confirm Password</label>
                    <input type="text" class="form-control" name="confirmPassword" id="confirmPassword" placeholder="EnterPassword.1234" required>
                </div>
                <div class="mb-3">
                    <label for="cinemaLocation" class="form-label">Address</label>
                    <input type="text" class="form-control" name="cinemaLocation" id="cinemaLocation" placeholder="Piazza S. Paolo All'Orto, 18, Pisa" >
                </div>
                <%
                    String registrationStatus = (String) request.getSession().getAttribute("registrationStatus");
                    if(registrationStatus != null && registrationStatus.equals("error")) {
                %>
                    <div class="alert alert-danger" role="alert">
                        Registration failed. Retry later.
                    </div>
                <%
                    }
                %>
                <input value="MAGIC" type="hidden">
                <button id="regButton" type="submit" class="btn btn-primary" >
                    Sign Up
                </button>
              </form>
              <div>
                <a href="<%=request.getContextPath()%>/CinemaLoginServlet">Sign IN (if already registered)</a>
              </div>
            </div>
          </div>
        </div>
    </body>
</html>
