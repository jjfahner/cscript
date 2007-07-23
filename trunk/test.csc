var precedence = (1 + 2) * (3 + 4);
print(precedence);

for(var a = 0; a < 10; ++a)
{
  print(("" + a) + " -");
}

var piet = "Hallo wereld" + "!!!";
var c, d;
c = d = piet;

var i, j, a, b;
i = "Hallo";
j = " wereld";
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
