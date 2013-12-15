(function($) {  
	$.fn.accordion = function(opt) {

		var defaults = {
			headerClass: 'h3',
			contentClass: 'ul',
			showFirst: true,
			speed: 'normal'
		};
		var opt = $.extend(defaults, opt);
		
		return this.each( function() {
			var obj = $(this);
			
			var lists = obj.find( opt.contentClass, obj);
			lists.each( function(i) {
				$(this).hide();
			});
			
			obj.find( opt.headerClass, obj ).each( function() {
				var igniter = $(this);				
				var anchor = makeToggleAnchor( igniter.text() );
				igniter.html( anchor );
				anchor.click( function() {
					var listToToggle = igniter.next( opt.contentClass );
					lists.each( function() { 
						$(this).slideUp(opt.speed);
					});
					if ( listToToggle.is(':visible') ) {
						listToToggle.slideUp(opt.speed);
					} else {
						listToToggle.slideDown(opt.speed);
					}
					return false;
				});
			});

		});
	};
	
	function makeToggleAnchor(text) {
		return $('<a href="#">' + text + '</a>');
	};
	
})(jQuery);