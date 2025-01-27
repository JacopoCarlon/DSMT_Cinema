let ws;

function connect(ctx, username, is_cinema) {
    let host = document.location.host;
    const url = "ws://" +host  + ctx + "/user_page_endpoint/" + username ;
    console.log("Connecting to main menu endpoint with url: " + url);
    ws = new WebSocket(url);

    ws.onmessage = function(event) {
        //Logic to remove message
        console.log("Arrived new auction list")
        var auctionListObject = JSON.parse(event.data);
        console.log(auctionListObject);
        console.log(auctionListObject.auctionList);
        console.log(auctionListObject.active)
        updateAuctionList(ctx, auctionListObject.auctionList, auctionListObject.active);
    };
}