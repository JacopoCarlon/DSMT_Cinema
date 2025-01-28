package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.OtpErlangMap;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;


public class Customer {
    String              username;
    String              password;


    public Customer(String username, String password) {
        this.username = username;
        this.password = password;
    }

    public String getUsername() {
        return username;
    }

    @Override
    public String toString() {
        return  "Customer{username=" + username  +
                ", password=" + password + "}\n";
    }

    public OtpErlangMap toOtpErlangMap() {
        return new OtpErlangMap(
                new OtpErlangObject[]{new OtpErlangString("username"), new OtpErlangString("password")},
                new OtpErlangObject[]{new OtpErlangString(username), new OtpErlangString(password)}
        );
    }

    public OtpErlangMap customerNameToOtpErlangMap(){
        return new OtpErlangMap(
                new OtpErlangObject[]{new OtpErlangString("username") },
                new OtpErlangObject[]{new OtpErlangString(username)}
        );
    }


}
