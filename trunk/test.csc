var a = {
  
  foo : function() 
  { 
    print("In foo:{baz}\n");
    ++baz;
  },

  bar : function() 
  { 
    print("In bar:{baz}\n"); 
    ++baz;
  },

  baz : 10

};

var b = {};

a.foo();
a.bar();

[ "yessir" : function() { print("yessir\n");} ].yessir();

dump();