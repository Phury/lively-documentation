(function($) {  
	$.fn.editor = function(opt) {

		var defaults = {
			editorClass: 'editor',
			editClass: 'edit',
			runClass: 'run',
			deleteClass: 'delete',
			runnable: 'code',
			visible: false
		};
		var opt = $.extend(defaults, opt);


		return this.each( function() {
			
			var obj = $(this);
			
			var editor = obj.find( 'div.'+opt.editorClass, obj);
			// No editor in the element, nothing to do
			if ( editor.length == 0 ) return;
			
			var buttonBar = editor.find( 'div.bbar', editor );
			var toolBar = editor.find( 'div.tbar', editor );
			
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
			
			var runAction =  makeEditorAction( opt.runClass );
			runAction.click( function() {
				if ( getType() != opt.runnable ) {
					return false;
				}
				
				if ( $.fn.runCode ) {
					var data = jQuery.param({code:getValue()});
					$.fn.runCode( data, function(result, textStatus) {
						if ( result ) {
							editor.before( result.notification() );
						}
					});
				}
				return false;
			});
			actions.append( runAction );
			
			// Add the actions at end of editor
			editor.after( actions );
			
			/* Save the documentation entry */
			var saveButton = buttonBar.find('> button.save', buttonBar);
			saveButton.click( function() {
				updateFieldsFromEditor();
				toggleEditor();
				return false;
			});
			
			/* Cancel modifications of documentation entry */
			var cancelButton = buttonBar.find('> button.cancel', buttonBar);
			cancelButton.click( function() {
				updateEditorFromFields();
				toggleEditor();
				return false;
			});
			
			/* Define a global documentation entry action */
			var globalCheckbox = toolBar.find('input:checkbox', toolbar);
			globalCheckbox.click( function() {
				var checkbox = $(this);
				var label = checkbox.parents().eq(0);
				if ( checkbox.is(':checked') ) {
					//var input = $('<input type="text" name="url" value="">');
					var addEntity = $('<label for="">Entity: <input type="text" name="ref" value=""></label>');
					var moreButton = $('<button type="button" class="more">attach another...</button>');
					moreButton.click( function() {
						moreButton.before( addEntity.clone() );
					});
					label.after( moreButton );
					label.after( addEntity );
				} else {
					label.nextAll().remove();
				}
				
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
					runAction.fadeIn('slow').fadeOut('slow').fadeIn('slow');
				} else {
					runAction.fadeOut('slow');
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
				obj.find('pre.value', obj).text( getValue() );
				obj.find('p.type', obj).text( getType() );
			};
			
			function updateEditorFromFields() {
				setName( obj.find('p.name').text() );
				setValue( obj.find('pre.value').text() );
				setType( obj.find('p.type').text() );
			};

		});
	};

	
	function makeEditorAction(name) {
		return $('<a href="'+name+'" class="'+name+'">'+name+'</a>');
	};
	
	
	$.fn.factory = function(opt) {
		var defaults = {
			wrapperClass: 'clonable-wrapper',
			clonableClass: 'clonable',
			factoryClass: 'factory-clonable',
			newClass: 'new'
		};
		var opt = $.extend(defaults, opt);


		return this.each( function() {
			var obj = $(this);
			
			var factoryElt = obj.find( '.'+opt.factoryClass, obj);
			// If there is no factory, nothing to do, just return
			if ( factoryElt.length == 0 ) return;
			
			// Else, hide the factory and add a button to copy them
			factoryElt.hide();
			
			var action = $('<div class="action"></div>');
			var newAction = makeEditorAction( opt.newClass );
			newAction.click( function() {
				var clone = factoryElt.clone();
				clone.removeClass(opt.factoryClass);
				clone.addClass(opt.clonableClass);
				clone.editor({visible: true});
				clone.show();
				factoryElt.before(clone);
				return false;
			});
			action.append( newAction );
			obj.append( action );
		});
	};
	
})(jQuery);