function foo(a)
{
  function bar(a)
  {
    include "test.csi";
  }
  return bar(a);
}

var f = foo;

print(f("Hello world"));