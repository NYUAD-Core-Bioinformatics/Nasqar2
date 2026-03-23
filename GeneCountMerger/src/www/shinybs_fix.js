$(document).ready(function () {
  if (typeof shinyBS === 'undefined' ||
      !shinyBS.inputBindings ||
      !shinyBS.inputBindings.collapse) return;

  // FIX 1: subscribe was passing the raw jQuery event as the callback arg.
  // Shiny 1.13 reads event.mode from it -> gets [object Object] -> crash.
  // Pass a plain boolean instead (the old allowDeferred API).
  shinyBS.inputBindings.collapse.subscribe = function (el, callback) {
    $(el).find(".collapse").on("shown.bs.collapse hidden.bs.collapse", function () {
      callback(true);
    });
  };

  // FIX 2: getValue returns a JS array; Shiny 1.13 rejects object values.
  // Return a comma string or null instead.
  var origGetValue = shinyBS.inputBindings.collapse.getValue;
  shinyBS.inputBindings.collapse.getValue = function (el) {
    var val = origGetValue.call(this, el);
    if (!val || (Array.isArray(val) && val.length === 0)) return null;
    if (Array.isArray(val)) return val.join(",");
    return val;
  };

  // FIX 3: add missing getRatePolicy so Shiny does not get undefined as mode
  shinyBS.inputBindings.collapse.getRatePolicy = function () { return null; };
});
