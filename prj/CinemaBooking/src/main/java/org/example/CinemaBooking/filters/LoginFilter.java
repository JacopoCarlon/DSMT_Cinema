package org.example.CinemaBooking.filters;

import jakarta.servlet.*;
import jakarta.servlet.annotation.WebFilter;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.util.Objects;

@WebFilter(filterName = "CheckLoginFilter", urlPatterns = {"/BrowseShowsPageServlet", "/CinemaPageServlet", "/CreateShowServlet", "/ShowPageServlet", "/UserPageServlet" })
public class LoginFilter implements Filter {

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        // Initialization logic, if needed
    }

    @Override
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain) throws ServletException, IOException {
        // Log the execution of the filter (for debugging purposes)
        System.out.println("LoginFilter");

        // Cast the generic ServletRequest and ServletResponse to HttpServletRequest and HttpServletResponse
        HttpServletRequest request = (HttpServletRequest) req;
        HttpServletResponse response = (HttpServletResponse) res;

        // Retrieve the session object, but do not create a new one if it doesn't exist (false parameter)
        HttpSession session = request.getSession(false);

        // Construct the login URI dynamically using the context path of the application
        String loginCustomerURI = request.getContextPath() + "/LoginServlet";
        String loginCinemaURI = request.getContextPath() + "/CinemaLoginServlet";

        // Check if the user is logged in:
        // - A session exists (session != null)
        // - The session contains a "username" attribute (session.getAttribute("username") != null)
        boolean LEGAL_loggedIn = session != null && session.getAttribute("username") != null && session.getAttribute("is_a_cinema") != null;
        if (LEGAL_loggedIn) {
            String is_cinema = (String) session.getAttribute("is_a_cinema");
            // // LEGAL_loggedIn = LEGAL_loggedIn && (Objects.equals(is_cinema, "true") || Objects.equals(is_cinema, "false"));
            LEGAL_loggedIn = Objects.equals(is_cinema, "true") || Objects.equals(is_cinema, "false");
        }

        // Check if the current request is for the login page itself (either customer or cinema)
        boolean loginCustomerRequest = request.getRequestURI().equals(loginCustomerURI);
        boolean loginCinemaRequest = request.getRequestURI().equals(loginCinemaURI);


        // If the user is logged in OR the request is for the login page, allow the request to proceed
        if (LEGAL_loggedIn || loginCustomerRequest || loginCinemaRequest) {
            // Pass the request and response along the filter chain
            // ALLOW the request to proceed !
            chain.doFilter(request, response);
        } else {
            // If the user is not logged in and the request is not for the login page, redirect to the login page
            // since the user is not logged, we should not assume that it is a cinema, so we send to customer login
            response.sendRedirect(loginCustomerURI);
        }
    }

    @Override
    public void destroy() {
        // Cleanup logic, if needed
    }
}
