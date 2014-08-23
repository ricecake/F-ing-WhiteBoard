(function() {
	'use strict';

	var Tool = ring.create({
		constructor: function(args) {
			this.data = args['data'] || {};
			this.name = args.name;
			this.loc  = args.location;
			return this;
		}
	});
	var Brush = ring.create([Tool], {
		constructor: function(args) {
			this.adjusters = args.adjusters;
			return this.$super(args)
		}
	});
	var Paint = ring.create([Brush], {
		constructor: function() {
			return this.$super({
				data: {
					color: 'rgba(00,00,00,1)',
					size:  5
				},
				name: 'Paint',
				location: ['Tools', 'Brush', 'Paint'],
				adjusters: {}
			});
			return this;
		}
	});

	

	var fingwb = {
		setMode: function(mode) {
			fingwb.mode[fingwb.mode.current].uninstall();
			fingwb.mode[mode].install();
		},
		global: {
			ws: '',
			canvas: {
				ctx: '',
				get: function (){ return $("#canvas")[0] },
				getContainer: function (){ return $("#canvas-container")[0] },
				getImageData: function() {
					var canvas = fingwb.global.canvas.get();
					return canvas.getContext('2d').getImageData(0,0, canvas.width, canvas.height);
				},
				putImageData: function (data, x, y) {
					x = x || 0;
					y = y || 0;
					var canvas = fingwb.global.canvas.get();
					return canvas.getContext('2d').putImageData(data, x, y);
				},
				setPixel: function setPixel(imageData, x, y, r, g, b, a) {
					index = (x + y * imageData.width) * 4;
					imageData.data[index+0] = r;
					imageData.data[index+1] = g;
					imageData.data[index+2] = b;
					imageData.data[index+3] = a;
				},
				getPixel: function setPixel(imageData, x, y) {
					index = (x + y * imageData.width) * 4;
					return [
						imageData.data[index+0],
						imageData.data[index+1],
						imageData.data[index+2],
						imageData.data[index+3]
						];
				}
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
					var canvas = $(fingwb.global.canvas.getContainer());
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
			fingwb.global.ws.send(JSON.stringify({clear: true}));
		});
		$('#size-control').noUiSlider({
			start: 5,
			step: 1,
			range: {
				'min': 1,
				'max': 150
			},
			serialization: {
				lower: [ $.Link({target: $('#size-display')})]
			}
		});
		$('#size-control').on({'slide': function() {
			fingwb.mode.draw.data.size = $(this).val();
		}});
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
	function clearCanvasNeedingCleverNameFromSebastian () {
		// Store the current transformation matrix
		var canvasWithCleverName = document.getElementById('system-overlay');
		var ctx = canvasWithCleverName.getContext('2d');
		ctx.save();
		// Use the identity matrix while clearing the canvas
		ctx.setTransform(1, 0, 0, 1, 0, 0);
		ctx.clearRect(0, 0, canvasWithCleverName.width, canvasWithCleverName.height);

		// Restore the transform
		ctx.restore();
	}

	$(document).on('mousemove', '#system-overlay', function(event) {
		trackMouse(event);
	});

	$(document).on('mouseout', '#system-overlay', function(event) {
		clearCanvasNeedingCleverNameFromSebastian(event);
	});

	function trackMouse(event) {
		var canvasWithCleverName = document.getElementById('system-overlay');
		var ctx = canvasWithCleverName.getContext('2d');
		ctx.globalCompositeOperation = "source-over";
		clearCanvasNeedingCleverNameFromSebastian();

		var selectedSize = fingwb.mode.draw.data.size;

		// Radius of circle, actual line is, apparently .5 the size of the arc that would be drawn using that size?
		var r = fingwb.mode.draw.data.size - (selectedSize * .5);
		var x;
		var y;


		// Yep, that's right, I totally stole Sebastian's code.. I'm ok with it.
		var cbr = canvasWithCleverName.getBoundingClientRect();
		var ex = event.clientX;
		var ey = event.clientY;
		x = (ex-cbr.left)/(cbr.right-cbr.left)*canvasWithCleverName.width;

		y = (ey-cbr.top)/(cbr.bottom-cbr.top)*canvasWithCleverName.height;

		// x = event.clientX - 8 + document.body.scrollLeft + document.documentElement.scrollLeft;
		// y = event.clientY - 8 + document.body.scrollTop + document.documentElement.scrollTop;

		ctx.strokeStyle = '#777';
		ctx.lineWidth = 3;
		ctx.beginPath();
		ctx.arc(x, y, r, 0, Math.PI * 2, true);
		ctx.closePath();
		ctx.fillStyle = fingwb.mode.draw.data.color;
		ctx.stroke();
		ctx.fill();
	};

}());
