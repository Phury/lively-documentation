function initEditable() { setEditableId(); }

/*(function($) {  
	$.fn.editable = function(options) {
		
		var defaults = {

		};
		var options = $.extend(defaults, options);

		return this.each(function() {
			
		};
	};
})(jQuery);*/

function setEditableId() {
	// Give a unique ID to each editable
	$('.editable').each( function(i) {
		var parentId = $(this).parents("div").eq(0).attr('id');
		$(this).attr( 'id', parentId + i );
		$(this).find('.editor').eq(0).attr('id', 'editor' + i);
	});
};

function addEditable(editableId) {
	var $editable = $('#'+editableId);
	var $newDiv = newEditable(editableId);
	$editable.append($newDiv);
	
	setEditableId();
	$('.editable').editor({visible: true});
	
	return false;
};

function delEditable(target) {
	$parentDivs = $(target).parents("div").eq(0).fadeOut("slow", function() {
		$(this).remove();
	});
	return false;
};

function newEditable(name) {
	return $('\
	<div class="editable">\
		<p class="name"></p>\
		<p class="value"></p>\
		<p class="type"></p>\
		<div class="editor"></div\
		<a href="#'+ name +'" class="edit" onclick="toggleEditor(this);">edit</a><a href="#'+ name +'" class="delete" onclick="delEditable(this);">delete</a>\
	</div><!-- end of editable -->');
};