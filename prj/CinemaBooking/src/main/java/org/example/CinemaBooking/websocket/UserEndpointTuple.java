package org.example.CinemaBooking.websocket;

public class UserEndpointTuple {
    String userID;
    String userType;
    ShowPageEndpoint endpoint;

    public UserEndpointTuple(String userID, String userType, ShowPageEndpoint endpoint) {
        this.userID = userID;
        this.userType = userType;
        this.endpoint = endpoint;
    }

    public String getUserID() {
        return userID;
    }

    public String getUserType() {
        return userType;
    }

    public ShowPageEndpoint getEndpoint() {
        return endpoint;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof UserEndpointTuple) {
            UserEndpointTuple other = (UserEndpointTuple) o;
            return this.userID.equals(other.userID) && this.userType.equals(other.userType);
        }
        return false;
    }
}
