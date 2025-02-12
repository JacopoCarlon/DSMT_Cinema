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

///////
let ws;

function connect(ctx, showID, is_a_cinema, userIdentifier) {
    let userType = is_a_cinema ? "cinema" : "customer";
    let host = document.location.host;
    const url = "ws://" + host + ctx +
        "/show_page_endpoint/" + showID + "/" +
        userType + "/" + userIdentifier;
    console.log("Connecting to UserPageEndpoint with url: " + url);
    ws = new WebSocket(url);

    ws.onmessage = function(event) {
        console.log("Arrived updated show state")
        let showState = JSON.parse(event.data);
        console.log(showState);
        updateShowState(ctx, is_a_cinema, userIdentifier, showState)
    };
}

function updateShowState(ctx, is_a_cinema, userIdentifier, showState) {
    const availableSeats = document.querySelector('#currAvailableSeats');
    availableSeats.value = showState.currAvailableSeats;

    const isEnded = document.querySelector('#isEnded');
    isEnded.value = showState.isEnded;

    const parentNode = document.querySelector('#changes_form_parent');
    while (parentNode.firstChild) { parentNode.removeChild(parentNode.firstChild); }

    parentNode.append( is_a_cinema ?
        createElementsForCinema(showState) :
        createElementsForCustomer(userIdentifier, showState)
    );
}


function createElementsForCinema(showState) {
    const tableTitle = document.createElement("h4");
    tableTitle.classList.add("d-flex", "justify-content-center", "p-3");


}

function createElementsForCustomer(userIdentifier, showState) {
    // TODO: do
}
