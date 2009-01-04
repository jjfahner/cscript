
function main()
{
  /*
  { Foo : function(h) { print("{h} {w}\n");}, w : "world" }.Foo("Hello");

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

  { Anonymous : function(arg) { print("Anonymous {arg}\n");} }.Anonymous("data");
  */
   var c = function(arg) { print(" -> {arg} <- \n"); };
   c("Hello");
}

main();