var r = 0;
try
{
  throw 1;
  r = 1;
}
catch(e)
{
  r = 2;
}
return r == 2;
