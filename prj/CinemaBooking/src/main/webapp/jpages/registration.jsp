<%--
  Created by IntelliJ IDEA.
  User: Admin
  Date: 11/12/2024
  Time: 17:46
  To change this template use File | Settings | File Templates.

  see : https://getbootstrap.com/docs/5.2/getting-started/download/#cdn-via-jsdelivr
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
  <title>Registration</title>
  <meta charset="utf-8">
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-rbsA2VBKQhggwzxH7pPCaAqO46MgnOM80zW1RWuH61DGLwZJEdK2Kadq2F9CUG65" crossorigin="anonymous">
  <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-kenU1KFdBIe4zVF0s0G1M5b4hcpxyD9F7jL+jjXkk+Q2h455rYXK/7HAuoJl+0I4" crossorigin="anonymous"></script>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Bootstrap demo</title>
</head>
<body>

<div class="container">
  <h1 class="justify-content-center d-flex">CinemaBooking - Registration Page</h1>
  <div class="card">
    <div class="card-body">
      <form method="POST" action="<%=request.getContextPath()%>/RegistrationServlet">
        <div class="mb-3">
          <label for="username" class="form-label">Username</label>
          <input type="text" class="form-control" name="username" id="username" placeholder="Pippo1234" pattern="^(?=.{8,20}$)(?![_.])(?!.*[_.]{2})[a-zA-Z0-9._]+(?<![_.])$" required>
        </div><div class="mb-3">
        <label for="password" class="form-label">Username</label>
        <input type="text" class="form-control" name="password" id="password" placeholder="EnterPassword.1234" required>
      </div><div class="mb-3">
        <label for="confirmPassword" class="form-label">Username</label>
        <input type="text" class="form-control" name="confirmPassword" id="confirmPassword" placeholder="EnterPassword.1234" required>
      </div>
        <input value="MAGIC" type="hidden">
        <button id="regButton" type="submit" class="btn btn-primary">
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
