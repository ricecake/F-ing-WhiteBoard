(function() {
	'use strict';
	var ws;
	var ctx;
	var color = '#000000';
	$(document).ready(init);

	var verbs = {
		draw: function(segment) {
			ctx.strokeStyle = segment.color;
			ctx.lineWidth = segment.width;
			ctx.beginPath();
			ctx.moveTo(segment.x[0], segment.y[0]);
			ctx.lineTo(segment.x[1], segment.y[1]);
			ctx.stroke();
		},
		join: function (viewer) {
			console.log(viewer);
		},
		leave:  function (viewer) {
			console.log(viewer);
		}
	};
	function init() {
		initWs();
		initCanvas();
	}
	
	function initWs() {
		var WhiteBoardId = $('body').data('whiteboard');
		ws = new WebSocket("ws://" + window.location.host + "/ws/" + WhiteBoardId);
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
		initWs();
	}

	function onMessage(e) {
		var message = JSON.parse(e.data);
		for (var k in message) {
			verbs[k](message[k]);
		}
	}

	function onError(e) {
		console.log(e);
	}

	function initCanvas() {
		ctx = $("#canvas")[0].getContext('2d');
		$("#canvas").drawTouch();
		$("#canvas").drawPointer();
		$("#canvas").drawMouse();
	}
	$.fn.drawTouch = function() {
		var segment = {
			x: [],
			y: []
		};
		var start = function(e) {
			e = e.originalEvent;
			segment.x[0] = e.changedTouches[0].pageX;
			segment.y[0] = e.changedTouches[0].pageY;
		};
		var move = function(e) {
			e.preventDefault();
			e = e.originalEvent;
			segment.x.push(e.changedTouches[0].pageX);
			segment.y.push(e.changedTouches[0].pageY);
			draw(segment);
			segment.x.shift();
			segment.y.shift();
		};
		$(this).on("touchstart", start);
		$(this).on("touchmove", move);
	};
	$.fn.drawPointer = function() {
		var segment = {
			x: [],
			y: []
		};
		var start = function(e) {
			e = e.originalEvent;
			segment.x[0] = e.pageX;
			segment.y[0] = e.pageY;
		};
		var move = function(e) {
			e.preventDefault();
			e = e.originalEvent;
			segment.x.push(e.pageX);
			segment.y.push(e.pageY);
			draw(segment);
			segment.x.shift();
			segment.y.shift();
		};
		$(this).on("MSPointerDown", start);
		$(this).on("MSPointerMove", move);
	};
	$.fn.drawMouse = function() {
		var segment = {
			x: [],
			y: []
		};
		var clicked = 0;
		var start = function(e) {
			clicked = 1;
			segment.x[0] = e.pageX;
			segment.y[0] = e.pageY;
		};
		var move = function(e) {
			if(clicked){
				segment.x.push(e.pageX);
				segment.y.push(e.pageY);
				draw(segment);
				segment.x.shift();
				segment.y.shift();
			}
		};
		var stop = function(e) {
			clicked = 0;
		};
		$(this).on("mousedown", start);
		$(this).on("mousemove", move);
		$(window).on("mouseup", stop);
	};
	function draw (segment) {
		segment.color = color;
		segment.width = 5;
		ws.send(JSON.stringify({draw: segment}));
	}
}());
