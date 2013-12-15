(function($) {  
	$.fn.editable = function(options) {
		
		var defaults = {
			selector: 'editable',
			delText: 'delete',
			newText: 'new'
		};
		var options = $.extend(defaults, options);

		return this.each(function(i) {
			var obj = $(this);
			
			var editables = $obj.find('.editable', obj);
			editables.each( function(i) {
				var id;
				var delAction = makeAnchor( id, opt.delText );
				delAction.click( function() {
					obj.find( id, obj ).fadeOut("slow", function() {
						$(this).remove();
					});
					return false;
				});
				$(this).append(delAction);
			});
			
			// Create an action to add a div whenever <new> is clicked
			var newAction = makeAnchor( id, opt.newText );
			newAction.click( function() {
				var newDiv = newEditable(id);
				obj.append(newDiv);

				setEditableId();
				return false;
			});
			
			// Create a div for the new action and add all to the editable object
			var newDiv = $('<div class="newEditable"></div>');
			newDiv.append(newAction);
			obj.append( newDiv );
		});		
	};
	
	function makeAnchor(target, text) {
		return $('<a href="#'+ target +'>'+text+'</a>');
	};
	
	function newEditable(name) {
		return $('\
		<div class="editable">\
			<p class="name"></p>\
			<p class="value"></p>\
			<p class="type"></p>\
			<div class="editor"></div\
			<a href="#'+ name +'">edit</a>\
		</div><!-- end of editable -->');
	};
	
})(jQuery);