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

  // Create temporary object with named function and invoke
  { foo : function(h) { print("{h} {w}\n");}, w : "world" }.foo("Hello");

  // Create temporary object with named function and invoke through index
  { foo : function(h) { print("{h} {w}\n");}, w : "world" }["foo"]("Hello");
  
  // Create temporary object with named function and invoke with function argument
  { foo : function(f) { print(f("world")); } } . foo ( function(arg) { return "Hello {arg}\n"; } );

  // Create temporary function and invoke
  function(h, w) { print("{h} {w}\n"); } (w : "world", h : "Hello");

  // Create temporary object with unnamed function and invoke through index
  [ function(h, w) { print("{h} {w}\n"); } ] [0] ("Hello", "world");

  //var f = function;
  
  function.a = "Hallo";
  print(function.name + " : " + function.a + "\n");

  print(function(){}.name + "\n");
}


main();

//function p(arg) { print(arg); return p; }
//p("Hello")(" ")("World");
