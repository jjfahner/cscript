// Precedence parsing check
if((1 - 2 - 3) != -4)
{
  print("Precedence check failed\n");
}

print("Precedence test: " + 10 + "\n");

// For test
for(var a = 0; a < 10000; ++a)
{
  var a = a + 1;
  if(a % 200 == 0)
  {
    print(".");
  }
}
print("\n");

// Precedence
print((1 == 0 ? "Not true" : 0 == 1 ? "Not true" : "True") + "\n");


var piet = "Hallo wereld" + "!!!";
var c, d;
c = d = piet;

var i, j, a, b;
i = "Hallo";
j = " wereld\n";
a = i + j;
print(a);

b = a;
a = 2 - 1;
a = 2 * 2;
a = 2 / 2;
a = 3 % 2;
i = ++a;
j = a++;
b = a;

a = 7 < 8;
a = 7 <= 8;
a = 7 == 7;
a = 7 != 8;
a = 8 >= 7;
a = 8 > 7;

