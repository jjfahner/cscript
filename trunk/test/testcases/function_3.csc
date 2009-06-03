var f = function(g) { return g(); };
return f(function() { return 1; }) == 1;