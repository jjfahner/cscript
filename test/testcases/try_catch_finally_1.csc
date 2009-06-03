var r = 0;
try
{
  throw 2;
  r = 1;
}
catch(e)
{
  r += e;
}
finally
{
  r += 1;
}
return r == 3;