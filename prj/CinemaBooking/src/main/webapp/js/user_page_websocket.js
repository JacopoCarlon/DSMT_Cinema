let ws;

function connect(ctx, username, is_cinema) {
    let host = document.location.host;
    const url = "ws://" +host  + ctx + "/user_page_endpoint/" + username ;
    console.log("Connecting to main menu endpoint with url: " + url);
    ws = new WebSocket(url);

    ws.onmessage = function(event) {
        //Logic to remove message
        console.log("Arrived new booking list")
        var bookingsListObject = JSON.parse(event.data);
        console.log(bookingsListObject);
        // idk if we will dynamically recreate the lists of bookings, we'll see
    };
}