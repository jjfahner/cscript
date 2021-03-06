function IsPrime(n)
{
  var nh = n / 2 + 1;
  for(var i = 2; i < nh; ++i)
  {
    if(n % i == 0)
    {
      return false;
    }
  }
  return true;
}

function TestPrime(number)
{
  var ticks = CScript.Ticks;
  var isPrime = IsPrime(number) ? "is" : "is not";
  ticks = CScript.Ticks - ticks;
  Console.WriteLn("{number} {isPrime} prime (in {ticks} ms)");
}

CScript.StartThread('TestPrime(15485867));
CScript.StartThread('TestPrime(15485863));
CScript.StartThread('TestPrime(15485861));
CScript.StartThread('TestPrime(15485869));

Console.WriteLn("Press any key to quit...");
Console.ReadChar();
