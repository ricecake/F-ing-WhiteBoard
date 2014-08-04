(function() {
	'use strict';
	var ws;
	var canvas;
	$(document).ready(init);

	function init() {
		alert("Start!");
		initWs();
		initCanvas();
	}
	
	function initWs() {
		ws = new WebSocket("ws://" + window.location.host + "/ws");
		ws.onopen = function(evt) { onOpen(evt) }; 
		ws.onclose = function(evt) { onClose(evt) }; 
		ws.onmessage = function(evt) { onMessage(evt) }; 
		ws.onerror = function(evt) { onError(evt) }; 
	}
	function onOpen(e) {
		console.log(e);
	}

	function onClose(e) {
		console.log(e);
	}

	function onMessage(e) {
		console.log(e);
	}

	function onError(e) {
		console.log(e);
	}

	function initCanvas() {

	}
}());
