(function($) {  
	$.fn.tagEditor = function(opt) {

		var defaults = {
			editorClass: 'tagEditor',
			tagList: 'ul',
			tagClass: 'ul li a',
			editClass: 'edit',
			deleteClass: 'delete',
			visible: false
		};
		
		var opt = $.extend(defaults, opt);

		return this.each( function() {
			var obj = $(this);
			
			var editor = obj.find( 'div.'+opt.editorClass, obj);
			var tagChoice = editor.find( 'div.tagChoice', tagChoice);
			var actions = $('<div class="action"></div>');
			
			// Add edit action that opens the editor
			var editAction = makeEditorAction( opt.editClass );
			editAction.click( function() {
				updateEditorFromFields();
				toggleEditor();
				return false;
			});
			actions.append( editAction );
			
			// Add delete action that remove the whole object
			var deleteAction = makeEditorAction( opt.deleteClass );
			deleteAction.click( function() {
				obj.fadeOut("slow", function() {
					$(this).remove();
				});
				return false;
			});
			actions.append( deleteAction );
			
			// Add actions behind editor div
			editor.after(actions);
			
			// Show tagEditor with opening animation if necessary
			if ( opt.visible ) {
				editor.show();
			} else {
				editor.hide();
			}

			// Set the data displayed into the tagEditor
			updateEditorFromFields();
			
			var saveButton = editor.find('div.bbar > button.save', editor);
			saveButton.click( function() {
				updateFieldsFromEditor();
				toggleEditor();
				return false;
			});
			
			var cancelButton = editor.find('div.bbar > button.cancel', editor);
			cancelButton.click( function() {
				updateEditorFromFields();
				toggleEditor();
				return false;
			});
			
			
			function toggleEditor() {
				if (editor.is(':visible')) {
					editor.hide();
				} else {
					editor.show();
				}
			};
			
			function updateFieldsFromEditor() {
				var newTagList = $('<ul></ul');
				tagChoice.find( ':checkbox', obj ).each(function() {
					var checkbox = $(this);
					if ( checkbox.is(':checked') ) {
						newTagList.append( makeTagItem(checkbox.val()) );
					}
				});
				
				var oldTagList = obj.find( opt.tagList, obj );
				oldTagList.replaceWith(newTagList);
			};
			
			function updateEditorFromFields() {
				var checkboxes = tagChoice.find( ':checkbox', tagChoice );
				var tags = obj.find( opt.tagClass, obj );
				
				checkboxes.each(function() {
					var checkbox = $(this);
					checkbox.attr('checked', false);
					tags.each( function() {
						var name = new String($(this).text()).toLowerCase();
						if ( checkbox.val()==name ) {
							checkbox.attr('checked', true);
						}
					});
				});
			};
			
		});
	};
	
	function makeTagItem(name) {
		return $('<li><a href="'+name+'">'+name+'</a></li>');
	};
	
	function makeEditorAction(name) {
		return $('<a href="edit" class="'+name+'">'+name+'</a>');
	};
	
})(jQuery);