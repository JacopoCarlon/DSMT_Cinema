let ws;

function connect(ctx, is_a_cinema, userIdentifier) {
    let type = is_a_cinema ? "cinema" : "customer";
    let host = document.location.host;
    const url = "ws://" + host + ctx + "/browse_shows_endpoint/" + type + "/" + userIdentifier;
    console.log("Connecting to UserPageEndpoint with url: " + url);
    ws = new WebSocket(url);

    ws.onmessage = function(event) {
        console.log("Arrived new show list")
        let showListObject = JSON.parse(event.data);
        console.log(showListObject);
        console.log(showListObject.showsList);
        updateShowList(ctx, showListObject.showsList);
    };
}

function updateShowList(ctx, showList){
    const parentNode = document.querySelector('#parent_show_list');

    while (parentNode.firstChild) { parentNode.removeChild(parentNode.firstChild); }

    showList.forEach(show=> {
            console.log("Single show: ");
            console.log(show)
            const form = createShowCard(ctx, show);
            parentNode.append(form)
    });
}


function createShowCard(ctx, show) {
    const form = document.createElement("form");
    form.classList.add("card",  "w-25")
    form.setAttribute("action", ctx + "/BrowseShowsServlet");
    form.setAttribute("method", "post");

    const main_div = document.createElement("div");
    main_div.classList.add("card-body",  "d-flex",  "flex-column", "justify-content-between", "p-3");

    const mid_div = document.createElement("div");

    const showID = document.createElement("input");
    showID.setAttribute("type", "hidden");
    showID.setAttribute("name", "showID");
    showID.value = show.showID;

    const title = document.createElement("h5");
    title.classList.add("card-title");
    title.innerHTML = show.showName

    const date_div = document.createElement("div");
    date_div.innerHTML = "showDate: " + show.showDate

    const cinema_div = document.createElement("div");
    cinema_div.innerHTML = "Cinema: " + show.cinemaName + ", in: " + show.cinemaLocation

    const seats_div = document.createElement("div");
    seats_div.innerHTML = "Available Seats: " + show.currAvailableSeats + "/" + show.maxSeats

    const last_elem = document.createElement("button");
    last_elem.classList.add("btn", "btn-primary", "m-3")
    last_elem.innerHTML = "Go to Show page"


    mid_div.append(showID, title, date_div, cinema_div, seats_div)
    main_div.append(mid_div, last_elem)
    form.append(main_div)
    return form
}

/*
<form class="card w-25" action="<%=request.getContextPath()%>/BrowseShowsServlet" method="post">
    <div class="card-body d-flex flex-column justify-content-between p-3">
        <div>
            <input type="hidden" name="showID" value="<%=this_show.getShowID()%>">
            <h5 class="card-title"><%=this_show.getShowName()%></h5>
            <div>showDate: <%=this_show.getShowDate()%></div>
            <div>Cinema: <%=this_show.getCinemaName()%>, in: <%=this_show.getCinemaLocation()%></div>
            <div>Available Seats: <%=this_show.getCurrAvailableSeats()%>/<%=this_show.getMaxSeats()%></div>
        </div>
        <button type="submit" class="btn btn-primary m-3">Go to Show page</button>
    </div>
</form>
 */
