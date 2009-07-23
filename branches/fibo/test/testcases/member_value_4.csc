var f = { foo : 1, bar : function() { return this.foo; } };
return f.bar() == 1;