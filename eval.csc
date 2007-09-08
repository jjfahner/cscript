function foo(a, b, c)
{
  return a + b * c;
}

var a = foo(1, 2, 3);
print("Hallo, het resultaat is " + a + "\n");

for(var a = 0; a < 10; ++a)
{
  print(a);
  if(a == 5) break;
}
