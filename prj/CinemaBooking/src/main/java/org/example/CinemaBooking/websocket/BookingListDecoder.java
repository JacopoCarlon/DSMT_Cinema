package org.example.CinemaBooking.websocket;

import jakarta.websocket.DecodeException;
import jakarta.websocket.Decoder;
import jakarta.websocket.EndpointConfig;
import org.example.CinemaBooking.dto.Booking;
import org.example.CinemaBooking.dto.BookingList;


public class BookingListDecoder implements Decoder.Text<BookingList>{
    private static Gson gson = new Gson();

    @Override
    public BookingList decode(String s) throws DecodeException {
        System.out.println("[AuctionListDecoder] Received: " + s);
        BookingList auctionList = gson.fromJson(s, BookingList.class);
        for(Booking auction: auctionList.getAuctionList())
            System.out.println(auction.toString());
        return auctionList;
    }

    @Override
    public boolean willDecode(String s) {
        return (s != null);
    }

    @Override
    public void init(EndpointConfig endpointConfig) {
        // Custom initialization logic
    }

    @Override
    public void destroy() {
        // Close resources
    }



}
