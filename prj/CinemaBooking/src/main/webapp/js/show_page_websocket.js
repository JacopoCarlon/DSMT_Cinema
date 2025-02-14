function check_valid_booking(){
    const des_maxSeats = document.querySelector('#des_maxSeats');
    const des_currAvSt = document.querySelector('#des_currAvailableSeats');
    const des_waiting_booking = document.querySelector('#des_waiting_booking');
    const trg_newBookN = document.querySelector('#new_booking_number');

    const maxSeats = parseInt(des_maxSeats.innerHTML);
    const currAvSt = parseInt(des_currAvSt.innerHTML);
    const oldBookN = parseInt(des_waiting_booking.innerHTML);
    const newBookN = parseInt(trg_newBookN.value);

    const offset = newBookN - oldBookN;

    let all_is_good = true;
    if( currAvSt > maxSeats ||
        maxSeats < 1 ||
        currAvSt < 0 ||
        offset > currAvSt ||
        newBookN < 0
    ){
        all_is_good = false;
        alert("mmmmmmm this is not gonna work buddy, something fishy going on");
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/HTMLObjectElement/setCustomValidity
    /* It's vital to set the message to an empty string if there are no errors.
        As long as the error message is not empty, the form will not pass validation and will not be submitted.
    */
    trg_newBookN.setCustomValidity((!all_is_good)? "bad choice of number buddy" : "");
    trg_newBookN.reportValidity();
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
        updateShowState(ctx, is_a_cinema, showState)
    };
}

function updateShowState(ctx, is_a_cinema, showState) {
    console.log("Updating show state");
    const showCard = document.querySelector('#current_show_card');
    while (showCard.firstChild) showCard.removeChild(showCard.firstChild);

    const title = document.createElement("h3");
    title.classList.add("d-flex", "justify-content-center", "p-3");
    title.innerHTML = showState.showName;

    const br = document.createElement("br");

    const info_div = document.createElement("div");
    info_div.classList.add("d-flex", "justify-content-center");

    const showDate = document.createElement("div");
    showDate.innerHTML = showState.showDate;

    const cinemaName = document.createElement("div");
    const cinemaNameLink = document.createElement("a");
    cinemaNameLink.href = ctx + "/CinemaPageServlet?cinemaID=" + showState.cinemaID;
    cinemaNameLink.innerHTML = showState.cinemaName;
    cinemaName.append(cinemaNameLink);

    const cinemaLocation = document.createElement("div");
    cinemaLocation.innerHTML = showState.cinemaLocation;

    const seatsDiv = document.createElement("div");
    const availableSpan = "<span id=id=\"des_currAvailableSeats\">" + showState.currAvailableSeats + "</span>";
    const maxSpan = "<span id=id=\"des_maxSeats\">" + showState.maxSeats + "</span>";
    seatsDiv.innerHTML = "Available Seats: " + availableSpan + "/" + maxSpan;

    const bookingDetails = (is_a_cinema) ?
        createElementsForCinema(showState):
        createElementsForCustomer(ctx, showState);

    info_div.append(br, showDate, br, cinemaName, br, cinemaLocation, br, seatsDiv, br)
    showCard.append(title, info_div, bookingDetails)
}

// Create table of bookings
function createElementsForCinema(showState) {
    const table = document.createElement("table");

    // table head
    const thead = document.createElement("thead");
    const theadRow = document.createElement("tr");

    const th1 = document.createElement("th");
    th1.innerText = "Customer";
    const th2 = document.createElement("th");
    th2.innerText = "Booking stored in Database";
    const th3 = document.createElement("th");
    th3.innerText = "Booking stored in Show node";

    theadRow.append(th1, th2, th3);
    thead.append(theadRow);
    table.append(thead);

    // table body
    const tbody = document.createElement("tbody");
    const mergedList = fullOuterJoin(showState.committedBookingsList, showState.waitingForCommitList);
    mergedList.forEach(function (value, key) {
        const tbodyRow = document.createElement("tr");

        const td1 = document.createElement("td");
        td1.innerText = key;
        const td2 = document.createElement("td");
        td2.innerText = value.committed.toString();
        const td3 = document.createElement("td");
        td3.innerText = value.waiting.toString();

        tbodyRow.append(td1, td2, td3);
        tbody.append(tbodyRow);
    });
    table.append(tbody);

    const parentNode = document.createElement("div");
    parentNode.classList.add("p-4", "d-flex", "flex-wrap");
    parentNode.append(table);
    return parentNode;
}

function createElementsForCustomer(ctx, showState) {
    const parentNode = document.createElement("div");
    parentNode.classList.add("p-4", "d-flex", "flex-wrap");

    const first_div = document.createElement("div")
    first_div.classList.add("d-flex","justify-content-center");
    first_div.append(document.createElement("br"));

    // committed bookings
    const committed_div =  document.createElement("div");
    let committed_value = 0;
    if (showState.committedBookingsList != null && showState.committedBookingsList > 0) {
        committed_value = showState.committedBookingsList[0].bookedSeats;
    }
    committed_div.innerHTML = "Committed Bookings: <span id=\"des_committed_booking\">" + committed_value + "</span>";
    first_div.append(committed_div);
    first_div.append(document.createElement("br"));

    // waiting bookings
    const waiting_div = document.createElement("div");
    let waiting_value = committed_value;
    if (showState.waitingForCommitList != null && showState.waitingForCommitList > 0) {
        waiting_value = showState.waitingForCommitList[0].bookedSeats;
    }
    else {
        waiting_div.setAttribute("style", "display: none;");
    }
    waiting_div.innerHTML = "New value waiting to be committed: <span id=\"des_waiting_booking\">" + waiting_value + "></span>";
    first_div.append(waiting_div);

    parentNode.append(first_div);

    // booking form
    if (!showState.isEnded) {
        const form = document.createElement("form");
        form.setAttribute("action", ctx + "/ShowPageServlet");
        form.setAttribute("method", "post");
        form.oninput = check_valid_booking;

        const form_main_div = document.createElement("div");
        form_main_div.classList.add("d-flex", "justify-content-between", "mb-3");

        const input_div = document.createElement("div");
        input_div.classList.add("mb-3");

        const label = document.createElement("label");
        label.classList.add("form-label");
        label.setAttribute("for", "new_booking_number");
        label.innerText = "Enter your new booking number:";

        const booking_input = document.createElement("input");
        booking_input.setAttribute("type", "number");
        booking_input.classList.add("form-control");
        booking_input.setAttribute("name", "new_booking_number");
        booking_input.id = "new_booking_number";
        booking_input.min = "0";
        booking_input.max = "144000000";
        booking_input.setAttribute("aria-describedby", "new_booking_number");
        booking_input.required = true;

        const button_div = document.createElement("div")

        const button = document.createElement("button");
        button.setAttribute("type", "submit");
        button.classList.add("btn", "btn-primary", "mx-2", "px-4");
        button.innerText = "Set this as new booking";

        input_div.append(label, booking_input);
        button_div.append(button);
        form.append(input_div, button_div);

        parentNode.append(form);
    }

    return parentNode;
}




//////////////////////////////////////////
// execute full outer join on two lists A and B of elements {customer: <username>, bookedSeats: <number>}
// where it is possible to match customerA with customerB
// returns Map<username, {committed: <number>, waiting: <number>}
function fullOuterJoin(committedBookings, waitingBookings) {
    const merged = new Map(); // Key -> { valueA, valueB }

    // Process list A
    for (const item of committedBookings) {
        merged.set(item.customer, {
            committed: item.bookedSeats,
            waiting: item.bookedSeats // now set to ValueA, if exists in listB, will be updated
        });
    }

    // Process list B
    for (const item of waitingBookings) {
        if (merged.has(item.customer)) {
            // If the key exists in listB, update valueB
            merged.get(item.customer).waiting = item.bookedSeats;
        } else {
            // If the key doesn't exist in listB, add it with valueA = 0
            merged.set(item.customer, {
                committed: 0,
                waiting: item.bookedSeats
            });
        }
    }
    return merged;
}