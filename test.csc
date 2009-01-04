include "test.csi";

function foo1(a){}
function foo2(a = null);
function foo3(a, b);
function foo4(a, b = null);
function foo5(a = null, b = null);

var f = function(a){ print("{a}\n"); };

if(true)
{
  1;
  2 == 2;
  "true";
}

// Precedence parsing check
if((1 - 2 - 3) != -4)
{
  print("Precedence check failed\n");
}

print("Precedence test: " + div(mul(10, 2), 4) + "\n");

// For test
for(var a = 0; a < 1000; a = a + 1)
{
  var a = a + 1;
  if(a % 20 == 0)
  {
    doPrint(".");
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

// Test array functionality
a = null;
a[1] = "Cool";
a[2] = "Stuff!\n";
print(a[1] + " " + a[2]);

// Test nested array functionality
a = [];
a[1]["a"] = "Even ";
a[2]["b"] = "cooler ";
a[3]["c"] = "stuff\n";
print(a[1]["a"] + a[2]["b"] + a[3]["c"]);

// Test while statement
print("while: ");
a[0][1] = 0;
while(a[0][1] < 10)
{
  doPrint(".");
  a[0][1] = a[0][1] + 1;
}
print("\n");

// List notation
var arr = [1, 2, 3, ["true", "false"], [[1], true, false]];
print(arr); print("\n");

for(var q = 0; q < 10000; ++q)
{
  if(q % 100 == 0)
  {
    print(".");
  }
}
print("\n");

for(var v in [1, 2, 3, [true, false], [1, [2, 3]], "Blabla"])
//for(var v in [1, 2, 3, true, false, "Blabla"])
{
  print("{v}\n");
}

print("\nDone\n\n");

dump();