(function($) {  
	$.fn.alphasorter = function(opt) {

		var defaults = {
			filterTitle: '',
			filterClass: 'alphasorter',
			filterAllName: 'all',
			filterableItem: 'ul li',
			filterPosition: 'h1'
		};
		var opt = $.extend(defaults, opt);
		var alpha = "abcdefghijklmnopqrstuvwxyz";
		
		return this.each( function() {
			var obj = $(this);
			
			var filters = makeFilterDiv( opt.filterClass, opt.filterTitle );
			var filterList = filters.find( 'ul', filters );
			
			// Add a filter to show all elements
			var action =  makeFilterAction( opt.filterAllName );
			action.find('a').click( function() {
				obj.find(opt.filterableItem ,obj).each( function() {
					$(this).show();
				});
				return false;
			});
			filterList.append(action);
			
			// For each letter found, add a new elt in the filters
			var items = obj.find(opt.filterableItem , obj);
			items.each( function () {
				var attr = $(this).find('a').text().charAt(0);
				
				if ( hasFilterable(attr) ) return;
				
				action = makeFilterAction( attr );
				action.find('a').click( function() {
					items.each( function() {
						if ( $(this).find('a').text().charAt(0) == attr ) {
							$(this).show();
						} else {
							$(this).hide();
						}
					});
					return false;
				});
				sortFilterAction(filterList, action);
			});
			
			// Add the filter div at the appropriate place
			obj.find( opt.filterPosition, obj ).after( filters );
			
			function hasFilterable(attr) {
				var listAnchors = filterList.find( 'a', filterList );
				for ( var i=0; i<listAnchors.length; i++ ) {
					if ( listAnchors.eq(i).text().charAt(0) == attr ) return true;
				}
				return false;
			};
			
		});
	};
	
	function sortFilterAction(domList, elt) {
		var letter = elt.find('a').text().charAt(0);
		domList.find('li').each( function() {
			if ( $(this).find('a').text().charAt(0) > letter  ) {
				$(this).before( elt );
			}
		});
	};
	
	function makeFilterDiv(classAttr, title) {
		return $('<div class="' + classAttr + '"><h3>' + title + '</h3><ul></ul></div>');
	};
	
	function makeFilterAction( name ) {
		return $('<li><a href="filterElts=' + name + '">' + name + '</a></li>');
	};
	
})(jQuery);