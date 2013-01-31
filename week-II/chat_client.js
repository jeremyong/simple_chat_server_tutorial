var websocket;

$(document).ready(function () {
    $("#connected").hide();
    $("#content").hide();
});

function connect()
{
    wsHost = $("#server").val()
    websocket = new WebSocket(wsHost);
    showScreen('<b>Connecting to: ' +  wsHost + '</b>');
    websocket.onopen = function(evt) { onOpen(evt) };
    websocket.onclose = function(evt) { onClose(evt) };
    websocket.onmessage = function(evt) { onMessage(evt) };
    websocket.onerror = function(evt) { onError(evt) };
};

function disconnect() {
    websocket.close();
    $("#connected").fadeOut();
    $("#content").fadeOut();
};

function toggle_connection(){
    if(websocket && websocket.readyState == websocket.OPEN){
        $("#connectbutton").html("connect");
        disconnect();
    } else {
        $("#connectbutton").html("disconnect");
        connect();
    };
};

function sendTxt() {
    if(websocket.readyState == websocket.OPEN){
        txt = $("#send_txt").val();
        websocket.send(txt);
        showScreen('sending: ' + txt);
    } else {
        showScreen('websocket is not connected');
    };
};

function onOpen(evt) {
    showScreen('<span style="color: green;">CONNECTED </span>');
    $("#connected").fadeIn('slow');
    $("#content").fadeIn('slow');
};

function onClose(evt) {
    showScreen('<span style="color: red;">DISCONNECTED </span>');
};

function onMessage(evt) {
    showScreen('<span style="color: blue;">RESPONSE: ' + evt.data+ '</span>');
};

function showScreen(txt) {
    $('#output').prepend('<p>' + txt + '</p>');
};

function clearScreen()
{
    $('#output').html("");
};
