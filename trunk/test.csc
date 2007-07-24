
// Precedence parsing check
if((1 - 2 - 3) != -4)
{
  print("Precedence check failed\n");
}

print("Precedence test: " + 10 + "\n");

// For test
for(var a = 0; a < 10; ++a)
{
  var a = a + 1;
  if(a % 2 == 0)
  {
    print(".");
  }
  else
  {
    print("+");
  }
  print(a % 2 ? "@" : "$");
}


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

