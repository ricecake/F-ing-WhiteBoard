(function() {
	'use strict';
	var ws;
	
	function initWs () {
		ws = new WebSocket("ws://" + window.location.host + "/ws");
		websocket.onopen = function(evt) { onOpen(evt) }; 
		websocket.onclose = function(evt) { onClose(evt) }; 
		websocket.onmessage = function(evt) { onMessage(evt) }; 
		websocket.onerror = function(evt) { onError(evt) }; 
	}
	function onOpen(e){}
	function onClose(e){}
	function onMessage(e){}
	function onError(e){}
}());
