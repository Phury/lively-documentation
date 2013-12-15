(function($) {
	$.fn.ldoc = function(opt) {

		return this.each( function() {
			var obj = $(this);
			
			$('#content form.entity-handler').postEntity();
			
			$('#content form.tag-handler').addTag();

		});
	};
	
	$.fn.notification = function(opt) {

		return this.each( function() {
			var notification = $(this);
			
			notification.hide();
			
			// Add a cross to close the notification
			var message = notification.find( 'p', notification );
			var close = $('<a href="close">x</a>');
			var cssObj = {
				'font-family': '"Arial", "sans-serif"',
				'float': 'right',
				'padding': '0 0.3em 0 0.3em',
				'color': notification.css('border-top-color')
			};
			close.css(cssObj);
			close.click( function() {
				notification.fadeOut("slow");
				return false;
			});
			message.append(close);
			
			// Blink
			$(this).fadeIn('slow').fadeOut('slow').fadeIn('slow');
		});
	};

	$.fn.postEntity = function(opt) {

		return this.each( function() {
			var form = $(this);
			
			form.submit( function() {
				var page = '?ajax-entity-handler';
				var data = form.serialize();
				$.post(page, data, function (result, textStatus) {
					/*if ( textStatus == "success" ) {
						alert( 'yeah' );
					} else {
						alert('Dhoo');
					}*/
					if ( result ) {
						var result = $(result);
						result.notification();
						form.append( result );
						//form.find( "button.save",form).after( result );
					}
				}, 'html');
				return false;
			});
		});
	};
	
	$.fn.addTag = function() {
		
		return this.each( function() {
			var form = $(this);
			
			form.submit( function() {
				var page = '?ajax-tag-handler';
				var data = form.serialize();
				$.post(page, data, function (result, textStatus) {
					if ( result ) {
						var result = $(result);
						result.notification();
						form.append( result );
					}
				}, 'html');
				return false;
			});
		});
		
	};
	
	$.fn.runCode = function(data, callback) {
		var page = '?ajax-run-handler';
		$.post(page, data, function (result, textStatus) {
			if (callback) {
				callback( $(result) , textStatus);
			}
		}, 'html');
		return false;
	};
	
})(jQuery);