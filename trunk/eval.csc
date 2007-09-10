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
print("\n");

switch(a)
{
case 0:
  print("0\n");
  break;
case 1:
  print("1\n");
  break;
case 7:
  print("7\n");
  break;
default:
  print("default\n");
  break;
}

a = [1, 2, 3];
print(a[2]);

for(var b in a)
{
  print(b);
}

print("\n");
