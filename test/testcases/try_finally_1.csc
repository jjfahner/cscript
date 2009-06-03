var r = 0;
try
{
  try
  {
    throw 1;
    r = 1;
  }
  finally
  {
    r = 2;
  }
}
catch(e)
{
}
return r == 2;