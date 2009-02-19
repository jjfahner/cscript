namespace foo 
{
  var a = 40;
  namespace bar
  {
    var a = 2;
  }
  namespace baz
  {
    function a()
    {
      return ::foo::a + ::foo::bar::a;
    }
  }
  function b()
  {
    return bar::a + baz::a();
  }
}

var a = 123;

print("" + a + "\n");
print("" + ::a + "\n");
print("" + foo::baz::a() + "\n");
print("" + foo::b() + "\n");
