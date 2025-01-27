<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
    <head>
      <title>Registration</title>
      <meta charset="utf-8">

        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>

      <meta name="viewport" content="width=device-width, initial-scale=1">
    </head>
    <body>
        <div class="container">
          <h1 class="justify-content-center d-flex">CinemaBooking - Registration Page</h1>
          <div class="card">
            <div class="card-body">
              <form method="POST" action="<%=request.getContextPath()%>/RegistrationServlet"
                oninput='confirmPassword.setCustomValidity(confirmPassword.value != password.value ? "Passwords do not match." : "")'>
                <div class="mb-3">
                    <label for="username" class="form-label">Username</label>
                    <input type="text" class="form-control" name="username" id="username" placeholder="Pippo1234" pattern="^(?=.{8,20}$)(?![_.])(?!.*[_.]{2})[a-zA-Z0-9._]+(?<![_.])$" required>
                </div>
                <div class="mb-3">
                    <label for="password" class="form-label">Username</label>
                    <input type="text" class="form-control" name="password" id="password" placeholder="EnterPassword.1234" required>
                </div>
                <div class="mb-3">
                    <label for="confirmPassword" class="form-label">Username</label>
                    <input type="text" class="form-control" name="confirmPassword" id="confirmPassword" placeholder="EnterPassword.1234" required>
                </div>
                <%
                    String registrationStatus = (String) request.getSession().getAttribute("registrationStatus");
                    if(registrationStatus != null && registrationStatus.equals("error")) {
                %>
                    <div class="alert alert-danger" role="alert">
                    There was an unexpected error, please retry later.
                    </div>
                <%
                    }
                %>
                <div>
                    <input type="checkbox" id="is_cinema" name="is_cinema" />
                    <label for="is_cinema">Are you a cinema (do not lie)</label>
                </div>
                <div class="mb-3">
                    <label for="confirmPassword" class="form-label">If you are a cinema, you must indicate your address ?</label>
                    <input type="text" class="form-control" name="cinemaLocation" id="cinemaLocation" placeholder="via Dio Ti Salvi, 0" >
                </div>
                <input value="MAGIC" type="hidden">
                <button id="regButton" type="submit" class="btn btn-primary" >
                    Do Register !
                </button>
              </form>
              <div>
                <a href="<%=request.getContextPath()%>/LoginServlet">Sign IN (if already registered)</a>
              </div>
            </div>
          </div>
        </div>
    </body>
</html>
