(function($) {  
	$.fn.extendable = function(opt) {

		var defaults = {
			list: 'ul',
			items: 'li',
			subListClass: 'extendable',
			openedText: 'less',
			closedText: 'more',
			visibleLength: 15
		};
		var opt = $.extend(defaults, opt);


		return this.each( function() {
			var obj = $(this);
			
			var originalList = obj.find(opt.list, obj).eq(0);
			var list = $('<'+ opt.list + '></'+ opt.list + '>');
			var subList = $('<'+ opt.list + ' class=' + opt.subListClass + '></'+ opt.list + '>');
			
			// Set visible elements in list and others in subList
			var items = obj.find(opt.list + ' ' + opt.items, obj);
			
			// Don't modify list if it does not contain enough elements
			if ( items.size() < opt.visibleLength ) return;
			
			items.each( function(i) {
				if ( i < opt.visibleLength) {
					list.append( $(this) );
				} else {
					subList.append( $(this) );
				}
			});

			
			// make anchor to toggle display of subList
			var anchor = makeToggleAnchor(opt.closedText);
			anchor.click( function() {
				if (subList.is(':visible')) {
					subList.slideUp('normal');
					anchor.text(opt.closedText);
				} else {
					subList.slideDown('normal');
					anchor.text(opt.openedText);
				}
				return false;
			});
						
			// Replace original content with list and sublist
			originalList.remove( opt.items );
			originalList.append(list.find(opt.items));
			originalList.append(subList);
			originalList.after(anchor);
			originalList.find( '.'+opt.subListClass, obj ).hide();
		});
	};
	
	function makeToggleAnchor(text) {
		return $('<a class="extendAction" href="#">' + text + '</a>');
	};
	
})(jQuery);