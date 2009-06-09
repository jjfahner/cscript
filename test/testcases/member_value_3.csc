var f = { foo : 2, bar : function () { return foo; } };
return f.bar() == 2;