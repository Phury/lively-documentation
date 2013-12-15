(function($) {  
	$.fn.filterable = function(opt) {

		var defaults = {
			filterTitle: 'filter:',
			filterClass: 'filter',
			filterAllName: 'all',
			filterableItem: 'ul li',
			filterPosition: 'h3',
		};
		var opt = $.extend(defaults, opt);
		
		return this.each( function() {
			var obj = $(this);
			
			var filters = makeFilterDiv( opt.filterClass, opt.filterTitle );
			var filterList = filters.find( 'ul', obj);
			
			// Add a filter to show all elements
			var action =  makeFilterAction( opt.filterAllName );
			action.find('a').click( function() {
				obj.find(opt.filterableItem ,obj).each( function() {
					$(this).fadeIn('normal');
				});
				return false;
			});
			filterList.append(action);
			
			// For each filterable class found, add a new elt in the filters
			var items = obj.find(opt.filterableItem ,obj);
			items.each( function () {
				var attr = $(this).find('a').attr('class');
				
				if ( hasFilterable( attr ) ) return;
				
				action = makeFilterAction( attr );
				action.find('a').click( function() {
					items.each( function() {
						if ( $(this).find('a').hasClass( attr ) ) {
							$(this).fadeIn('normal');
						} else {
							$(this).fadeOut('normal');
						}
					});
					return false;
				});
				filterList.append(action);
			});
			
			// Add the filter div at the appropriate place
			obj.prev( opt.filterPosition ).after( filters );
			
			function hasFilterable(attr) {
				/* Not efficient, loop over all elements even if a correct attr is found */
				if ( !attr || attr == "" ) return true;
				var hasFilterAttr = false
				filterList.find( 'a' ).each( function() {
					if ( $(this).text() == attr ) hasFilterAttr = true;
				})
				return hasFilterAttr;
			};
			
		});
	};
	
	function makeFilterDiv(classAttr, title) {
		return $('<div class="' + classAttr + '"><h3>' + title + '</h3><ul></ul></div>');
	};
	
	function makeFilterAction( name ) {
		return $('<li><a href="filterElts=' + name + '">' + name + '</a></li>');
	};
	
})(jQuery);