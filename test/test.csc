namespace foo 
{
  namespace bar
  {
    var a = 10;
  }
  namespace baz
  {
    function a() 
    {
      return ::foo::bar::a;
    }
  }
}

print(foo::baz::a());