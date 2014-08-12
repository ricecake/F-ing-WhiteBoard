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
			var id = str2Col(viewer);
			$('<div/>', {
				'id': id,
				'class': 'viewer'
			}).css({
				'background-color': "#" + id,
			}).appendTo('#viewers');
		},
		leave:  function (viewer) {
			var id = str2Col(viewer);
			$('#' + id).remove();
		}
	};
	
	
	function init() {
		initWs();
		initCanvas();
	}
	
	function initWs() {
		var WhiteBoardId = $('body').data('whiteboard');
		ws = new WebSocket("ws://" + window.location.host + "/ws/" + WhiteBoardId);
		ws.onopen    = function(evt) { onOpen(evt);    };
		ws.onclose   = function(evt) { onClose(evt);   };
		ws.onmessage = function(evt) { onMessage(evt); };
		ws.onerror   = function(evt) { onError(evt);   };
	}
	function onOpen(e) {
		console.log(e);
	}

	function onClose(e) {
		window.setTimeout(function(){
			initWs();
		}, 1000);
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
			segment.x[0] = e.pageX-10;
			segment.y[0] = e.pageY-10;
		};
		var move = function(e) {
			if(clicked){
				// Technically moving right results in the line being 'behind' the cursor while
				// moving left results in the line just slightly ahead of the cursor
				segment.x.push(e.pageX-10);
				segment.y.push(e.pageY-10);
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
	function str2Col (str) {
		var i, hash, colour;
		for (i = 0, hash = 0; i < str.length; hash = str.charCodeAt(i++) + ((hash << 5) - hash));
		for (i = 0, colour = ""; i < 3; colour += ("00" + ((hash >> i++ * 8) & 0xFF).toString(16)).slice(-2));
		return colour;
	}
}());
