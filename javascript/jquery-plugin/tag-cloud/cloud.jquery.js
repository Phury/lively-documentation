(function($) {  
	$.fn.tagCloud = function(opt) {

		var defaults = {
			minPercent: 80,
			maxPercent: 180
		};
		var opt = $.extend(defaults, opt);


		return this.each( function() {
			var obj = $(this);
			
			
			var maxPercent = opt.maxPercent, minPercent = opt.minPercent;
			var max = 1, min = 999, count = 0;
			
			var items = obj.find('ul li', obj);
			
			items.each( function(i) {
				count = parseInt( $(this).find('span').text() );
				max = (count > max ? count : max);
				min = (min > count ? count : min);
			});
			
			var total, link, size;
			var multiplier = (maxPercent-minPercent)/(max-min);
		
			items.each( function(i) {
				span = $(this).find('span');
				count = parseInt( span.text() );
				size = minPercent + ((max-(max-(count-min)))*multiplier)+'%';
				$(this).css('font-size', size);
				span.remove();
			});
		});
	}

	
})(jQuery);