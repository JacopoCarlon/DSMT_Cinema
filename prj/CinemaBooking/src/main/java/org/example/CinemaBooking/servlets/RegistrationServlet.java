package org.example.CinemaBooking.servlets;

import org.example.CinemaBooking.communication.JE_CommunicationHandler;
import org.example.CinemaBooking.dto.Customer;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;

@WebServlet(name = "RegistrationServlet", value = "/RegistrationServlet")
public class RegistrationServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.out.println("doGet");
        request.getSession().removeAttribute("registrationStatus");

        RequestDispatcher requestDispatcher = request.getRequestDispatcher("/jpages/registration.jsp");
        requestDispatcher.forward(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String username = request.getParameter("username");
        String password = request.getParameter("password");

        System.out.println("DoPost Registration");
        System.out.println("username: " + username + "password: " + password);

        boolean isSignUpOkay = false;
        try {
            isSignUpOkay = new JE_CommunicationHandler().registerNewCustomer(request.getSession(), username, password);
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            e.printStackTrace();
        }

        if (isSignUpOkay){
            System.out.println("Registration success");
            response.sendRedirect(request.getContextPath() + "/LoginServlet");
        } else {
            System.out.println("Sign in failed");
            // show error in html
            request.getSession().setAttribute("registrationStatus", "error");
            RequestDispatcher requestDispatcher = request.getRequestDispatcher("/jpages/registration.jsp");
            requestDispatcher.forward(request, response);
        }
    }
}
