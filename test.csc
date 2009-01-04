
function main()
{
  // Create named object with methods like javascript
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

  // Invoke methods on object
  a.foo();
  a.bar();

  // Create variable containing function and invoke
  var c = function(arg) { print("Hello {arg}\n"); };
  c("world");

  // Create temporary object with function and invoke
  { Foo : function(h) { print("{h} {w}\n");}, w : "world" }.Foo("Hello");
  
  // Create temporary object with function and invoke with function argument
  { foo : function(f) { print(f("world")); } } . foo ( function(arg) { return "Hello {arg}\n"; } );
}

main();