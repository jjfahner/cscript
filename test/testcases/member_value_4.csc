var f = { foo : 2, bar : function() { return this.foo; } };
return f.bar() == 1;