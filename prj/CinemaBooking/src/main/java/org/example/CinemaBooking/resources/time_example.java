package org.example.CinemaBooking.resources;

import java.sql.Timestamp;
import java.text.SimpleDateFormat;

public class time_example
{
    public static void main(String[] args) {
        System.out.println("Hello World");

        String string = "Sun Oct 05 20:59:57 BRT 2014";
        String string2 = "2024-12-20T01:59";
        SimpleDateFormat formatter = new SimpleDateFormat("EEE MMM dd HH:mm:ss z yyyy");
        SimpleDateFormat formatter2 = new SimpleDateFormat("yyyy-mm-ddTHH:mm");
        try{
            java.sql.Timestamp datetime = new Timestamp(formatter.parse(string).getTime());
            System.out.println("DateTime: " + datetime.toString());

            java.sql.Timestamp datetime2 = new Timestamp(formatter2.parse(string2).getTime());
            System.out.println("DateTime: " + datetime2.toString());
        }
        catch(Exception e){
            System.out.println("ciao");
        }



    }



}
