var websocket;

$(document).ready(function () {
    $("#manage_connection input").focus();
    $("#connected").hide();
    $("#content").hide();
    $("#server").val("ws://localhost:7777");
    $("#connected").keypress(function(e) {
        if(e.which == 13) {
            e.preventDefault();
            e.stopPropagation();
            $("#send_txt_btn").click();
            $("#connected input").val("");
        }
    });
    $("#manage_connection").keypress(function(e) {
        if(e.which == 13) {
            e.preventDefault();
            e.stopPropagation();
            $("#connectbutton").click();
        }
    });
    $("#username").change(function(e) {
        update_username();
    });
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

function prompt_username() {
    var name = window.prompt("Please enter your username.", "john doe");
    $("#username").val(name);
}

function update_username() {
    var name = $("#username").val();
    if(websocket.readyState == websocket.OPEN){
        websocket.send("user:" + name);
    } else {
        showScreen('websocket is not connected');
    };
}

function disconnect() {
    websocket.close();
    $("#connected").fadeOut();
    $("#content").fadeOut();
    $("#manage_connection input").focus();
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
    } else {
        showScreen('websocket is not connected');
    };
};

function onOpen(evt) {
    showScreen('<span style="color: green;">CONNECTED </span>');
    prompt_username();
    update_username();
    $("#connected").fadeIn('slow');
    $("#content").fadeIn('slow');
    $("#connected input").focus();
};

function onClose(evt) {
    showScreen('<span style="color: red;">DISCONNECTED </span>');
};

function onMessage(evt) {
    showScreen('<span style="color: blue;">' + evt.data + '</span>');
};

function showScreen(txt) {
    //$('#output').prepend('<p>' + txt + '</p>');
    var tab = $('#output table');
    tab.prepend('<tr><td>' + txt + '</td></tr>');
    //$('#output table').scrollTop(tab.height());
};

function clearScreen()
{
    $('#output').html("");
};
