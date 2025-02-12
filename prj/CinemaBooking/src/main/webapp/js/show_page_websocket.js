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


/*
example of expected input :
const A = [
    { Key: "user1", value: 10 },
    { Key: "user2", value: 20 },
    { Key: "user3", value: 30 },
    { Key: "user4", value: 3000 }
];

const B = [
    { Key: "user2", value: 200 },
    { Key: "user3", value: 300 },
    { Key: "user5", value: 4000 }
];

ouotput :
const result = fullOuterJoin(A, B);
console.log(result);

Map(5) {
  'user1' => { Key: 'user1', valueA: 10, valueB: 10 },
  'user2' => { Key: 'user2', valueA: 20, valueB: 200 },
  'user3' => { Key: 'user3', valueA: 30, valueB: 300 },
  'user4' => { Key: 'user4', valueA: 3000, valueB: 3000 },
  'user5' => { Key: 'user5', valueA: 0, valueB: 4000 }
}
*/

// execute full outer join (modified) on two lists A and B of type <Key, value>
// where it is possible to match KeyA with KeyB
function fullOuterJoin(A, B) {
    const merged = new Map(); // Key: string -> { Key, valueA, valueB }

    // Process list A
    for (const item of A) {
        merged.set(item.Key, {
            Key: item.Key,
            valueA: item.value,
            valueB: item.value // now set to ValueA, if exists in listB, will be updated
        });
    }

    // Process list B
    for (const item of B) {
        if (merged.has(item.Key)) {
            // If the key exists in listB, update valueB
            merged.get(item.Key).valueB = item.value;
        } else {
            // If the key doesn't exist in listB, add it with valueA = 0
            merged.set(item.Key, {
                Key: item.Key,
                valueA: 0,
                valueB: item.value
            });
        }
    }
    return merged;
}





function createElementsForCinema(showState) {
    const tableTitle = document.createElement("h4");
    tableTitle.classList.add("d-flex", "justify-content-center", "p-3");


}

function createElementsForCustomer(userIdentifier, showState) {
    // TODO: do
}
