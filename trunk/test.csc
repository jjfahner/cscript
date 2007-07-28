function doPrint(a)
{
  print(a);
  return;
  //return a;
}

function mul(a, b)
{
  return a*b;
}

//var g;
//return g;

function div(a, b)
{
  return a/b;
}

// Precedence parsing check
if((1 - 2 - 3) != -4)
{
  print("Precedence check failed\n");
}

print("Precedence test: " + div(mul(10, 2), 4) + "\n");

// For test
for(var a = 0; a < 10000; ++a)
{
  var a = a + 1;
  if(a % 200 == 0)
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
a[1] = "Cool";
a[2] = "Stuff!\n";
print(a[1] + " " + a[2]);

// Test nested array functionality
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
  ++a[0][1];
}
print("\n");