let ws;

function connect(ctx, username, is_cinema) {
    let host = document.location.host;
    const url = "ws://" +host  + ctx + "/show_page_endpoint/" + username ;
    console.log("Connecting to UserPageEndpoint with url: " + url);
    ws = new WebSocket(url);

    ws.onmessage = function(event) {
        //Logic to remove message
        console.log("Arrived new booking list")
        var bookingsListObject = JSON.parse(event.data);
        console.log(bookingsListObject);
        // idk if we will dynamically recreate the lists of bookings, we'll see
    };
}



function check_valid_booking(){
    var all_is_good = true;

    des_testUN0 = document.querySelector('#des_testUN0');
    des_testUN1 = document.querySelector('#des_testUN1');

    testUN0 = des_testUN0.value;
    testUN1 = des_testUN1.value;
    if(testUN0 != testUN1){
        all_is_good = false;
        alert("mmmmm sus transaction (you are not you)");
    }

    des_maxSeats = document.querySelector('#des_maxSeats');
    des_currAvSt = document.querySelector('#des_currAvailableSeats');
    des_prvBookN = document.querySelector('#des_num_seats');
    trg_newBookN = document.querySelector('new_booking_number');

    maxSeats = parseInt(des_maxSeats.value);
    currAvSt = parseInt(des_currAvSt.value);
    prvBookN = parseInt(des_prvBookN.value);
    newBookN = parseInt(trg_newBookN.value);

    if( currAvSt > maxSeats ||
        prvBookN > maxSeats ||
        newBookN > maxSeats ||
        maxSeats < 1 ||
        currAvSt < 0 ||
        // prvBookN < 0 ||
        newBookN < 0
    ){
        all_is_good = false;
        alert("mmmmmmm this is not gonna work buddy, something fishy going on");
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/HTMLObjectElement/setCustomValidity
    /* It's vital to set the message to an empty string if there are no errors.
        As long as the error message is not empty, the form will not pass validation and will not be submitted.
    */

    bid.setCustomValidity((all_is_good)? "bad choice of number buddy" : "");
    bid.reportValidity();

}
