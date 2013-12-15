(function($) {  
	$.fn.editor = function(opt) {

		var defaults = {
			editorClass: 'editor',
			editClass: 'edit',
			deleteClass: 'delete',
			runnable: 'code',
			visible: false
		};
		var opt = $.extend(defaults, opt);


		return this.each( function() {
			var obj = $(this);
			
			var editor = obj.find( 'div.'+opt.editorClass, obj);
			var actions = $('<div class="action"></div>');
			var select = editor.find( 'select', editor ).eq(0);
			select.change( function() {
				toggleRun();
			});
			
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
			
			// Add the actions at end of editor
			editor.after( actions );
			
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
			
			var runButton = editor.find('div.bbar > button.run', editor);
			runButton.click( function() {
				if ( getType() != opt.runnable ) {
					return false;
				}
				alert( $('form').serialize() );
				alert( 'running code: \n' + getValue() );
				return false;
			});
			
			// Set the data displayed into the editor
			updateEditorFromFields();
			toggleRun();
			
			// Show editor with opening animation if necessary
			if ( opt.visible ) {
				editor.show();
			} else {
				editor.hide();
			}
			
			function toggleEditor() {
				if (editor.is(':visible')) {
					editor.hide();
				} else {
					editor.show();
				}
			};
			
			function toggleRun() {
				if ( getType() == opt.runnable ) {
					runButton.fadeIn('slow').fadeOut('slow').fadeIn('slow');
				} else {
					runButton.fadeOut('slow');
				}
			};
			
			function getType() {
				return select.val();
			};
			
			function setType(value) {
				select.val(value);
			};
			
			function getName() {
				return editor.find('input').val();
			};
			
			function setName(data) {
				editor.find('input').val(data);
			};
			
			function getValue() {
				return editor.find('textarea').val();
			};

			function setValue(data) {
				editor.find('textarea').val(data);
			};
			
			function updateFieldsFromEditor() {
				obj.find('p.name', obj).text( getName() );
				obj.find('p.value', obj).text( getValue() );
				obj.find('p.type', obj).text( getType() );
			};
			
			function updateEditorFromFields() {
				setName( obj.find('p.name').text() );
				setValue( obj.find('p.value').text() );
				setType( obj.find('p.type').text() );
			};

		});
	};

	
	function makeEditorAction(name) {
		return $('<a href="'+name+'" class="'+name+'">'+name+'</a>');
	};
	
	
	$.fn.editorFactory = function(opt) {
		var defaults = {
			wrapperClass: 'clonable-wrapper',
			clonableClass: 'clonable',
			factoryClass: 'factory-clonable',
			newClass: 'new'
		};
		var opt = $.extend(defaults, opt);


		return this.each( function() {
			var obj = $(this);
			
			var factory = obj.find( '.'+opt.factoryClass, obj);
			factory.hide();
			
			var action = $('<div class="action"></div>');
			var newAction = makeEditorAction( opt.newClass );
			newAction.click( function() {
				var clone = factory.clone();
				clone.removeClass(opt.factoryClass);
				clone.addClass(opt.clonableClass);
				clone.editor({visible: true});
				clone.show();
				factory.before(clone);
				return false;
			});
			action.append( newAction );
			obj.append( action );
		});
	};
	
})(jQuery);