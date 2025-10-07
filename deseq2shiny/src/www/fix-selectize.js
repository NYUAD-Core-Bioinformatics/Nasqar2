// Fix for selectize mode error behind reverse proxy
// This must run BEFORE Shiny initializes selectize
(function() {
  console.log('Selectize fix loaded');
  
  // Intercept at the earliest possible moment - before DOM ready
  if (typeof $ !== 'undefined') {
    // Override selectize plugin registration to remove problematic a11y plugin
    var originalFn = $.fn.selectize;
    $.fn.selectize = function(options) {
      console.log('Intercepting selectize initialization', this.attr('id'));
      
      // Remove plugins that might cause issues
      if (options && options.plugins) {
        console.log('Original plugins:', options.plugins);
        options.plugins = options.plugins.filter(function(p) {
          return p !== 'selectize-plugin-a11y';
        });
        console.log('Filtered plugins:', options.plugins);
      }
      
      // Ensure mode is correct for multiple selects
      if (this.prop('multiple')) {
        options = options || {};
        options.mode = 'multi';
        console.log('Set mode to multi for', this.attr('id'));
      }
      
      // Call original
      return originalFn.call(this, options);
    };
  }
})();
