(function() {
	'use strict';
	var fingwb = {
		setMode: function(mode) {
			fingwb.mode[fingwb.mode.current].uninstall();
			fingwb.mode[mode].install();
		},
		global: {
			ws: '',
			canvas: {
				ctx: '',
				get: function (){ return $("#canvas")[0] }
			}
		},
		mode: {
			current: 'none',
			start: 'draw',
			draw: {
				data: {
					color: 'rgba(00,00,00,1)',
					size: 5
				},
				install: function() {
					var canvas = $(fingwb.global.canvas.get());
					(function() {
						var segment;
						canvas.on("touchstart.fingwb.draw", function(e) {
							e = e.originalEvent;
							segment = fingwb.mode.draw.startSegment(e.changedTouches[0].pageX, e.changedTouches[0].pageY);
						});
						canvas.on("touchmove.fingwb.draw", function(e) {
							e.preventDefault();
							e = e.originalEvent;
							segment = fingwb.mode.draw.addSegment(segment, e.changedTouches[0].pageX, e.changedTouches[0].pageY);
						});
					})();
					(function() {
						var segment;
						var start = function(e) {
							e = e.originalEvent;
							segment = fingwb.mode.draw.startSegment(e.pageX, e.pageY);;
						};
						var move = function(e) {
							e.preventDefault();
							e = e.originalEvent;
							segment = fingwb.mode.draw.addSegment(segment, e.pageX, e.pageY);
						};
						canvas.on("MSPointerDown.fingwb.draw", start);
						canvas.on("MSPointerMove.fingwb.draw", move);
					})();
					(function() {
						var segment;
						var clicked = 0;
						canvas.on("mousedown.fingwb.draw", function(e) {
							clicked = 1;
							segment = fingwb.mode.draw.startSegment(e.pageX, e.pageY);
						});
						canvas.on("mousemove.fingwb.draw", function(e) {
							if(clicked){
								segment = fingwb.mode.draw.addSegment(segment, e.pageX, e.pageY);
							}
						});
						$(window).on("mouseup.fingwb.draw", function(e) {
							clicked = 0;
						});
					})();
				},
				uninstall: function() {
					var canvas = $(fingwb.global.canvas.get());
					$(window).off('mouseup.fingwb.draw');
					var events = ['mouseup', 'mousedown', 'mousemove', 'MSPointerMove', 'MSPointerDown', 'touchmove', 'touchstart'];
					for (var i in events) {
						canvas.off(i +'.fingwb.draw');
					}
				},
				startSegment: function(x, y) {
					return {
						x: [x],
						y: [y]
					};
				},
				addSegment: function(seg, x, y) {
					seg.x.push(x);
					seg.y.push(y);
					fingwb.mode.draw.draw(seg);
					seg.x.shift();
					seg.y.shift();
					return seg;
				},
				draw: function(event) {
					var segment = {
						color: fingwb.mode.draw.data.color,
						width: fingwb.mode.draw.data.size,
						x: [],
						y: []
					};
					//x: (evt.clientX-rect.left)/(rect.right-rect.left)*canvas.width,
					//y: (evt.clientY-rect.top)/(rect.bottom-rect.top)*canvas.height
					var canvas = fingwb.global.canvas.get();
					var cbr = canvas.getBoundingClientRect();
					for (var i=0; i<event.x.length; i++) {
						var ex = event.x[i];
						segment.x[i] = (ex-cbr.left)/(cbr.right-cbr.left)*canvas.width;
					}
					for (var i=0; i<event.y.length; i++) {
						var ex = event.y[i];
						segment.y[i] = (ex-cbr.top)/(cbr.bottom-cbr.top)*canvas.height;
					}
					fingwb.global.ws.send(JSON.stringify({draw: segment}));
				}
			},
			none: {
				install:   function(){},
				uninstall: function(){}
			}
		},
		verbs: {
			draw: function(segment) {
				fingwb.global.canvas.ctx.strokeStyle = segment.color;
				fingwb.global.canvas.ctx.lineWidth   = segment.width;
				fingwb.global.canvas.ctx.beginPath();
				fingwb.global.canvas.ctx.moveTo(segment.x[0], segment.y[0]);
				fingwb.global.canvas.ctx.lineTo(segment.x[1], segment.y[1]);
				fingwb.global.canvas.ctx.stroke();
			},
			clear: function() {
				var canvas = fingwb.global.canvas.get();
				fingwb.global.canvas.ctx.clearRect(0, 0, canvas.width, canvas.height);
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
		}		
	};
	$(document).ready(init);
	
	function init() {
		initWs();
		initCanvas();
	}
	
	function initWs() {
		var WhiteBoardId = $('body').data('whiteboard');
		var ws = new WebSocket("ws://" + window.location.host + "/ws/" + WhiteBoardId);
		ws.onclose   = function(evt) {
			window.setTimeout(function(){
				initWs();
			}, 1000);
		};
		ws.onmessage = 	function(e) {
			var message = JSON.parse(e.data);
			for (var k in message) {
				fingwb.verbs[k](message[k]);
			}
		};
		ws.onerror   = 	function(e) {
			console.log(e);
		};
		fingwb.global.ws = ws;
	}

	function initCanvas() {
		$('#clear-canvas').on('click', function() {
			ws.send(JSON.stringify({clear: true}));
		});
		$('#size-control').on('change', function() {
			size = $(this).val();
		})
		$('#colorpicker').spectrum({
			clickoutFiresChange: true,
			showPalette: true,
                        showAlpha: true,
			change: function(newColor) {
				fingwb.mode.draw.data.color = newColor.toRgbString();
			},
			move: function(newColor) {
				fingwb.mode.draw.data.color = newColor.toRgbString();
			}
		});
		fingwb.global.canvas.ctx = fingwb.global.canvas.get().getContext('2d');
		fingwb.global.canvas.ctx.lineCap = "round";

		fingwb.setMode('draw');
	}
	function str2Col (str) {
		var i, hash, colour;
		for (i = 0, hash = 0; i < str.length; hash = str.charCodeAt(i++) + ((hash << 5) - hash));
		for (i = 0, colour = ""; i < 3; colour += ("00" + ((hash >> i++ * 8) & 0xFF).toString(16)).slice(-2));
		return colour;
	}
}());
