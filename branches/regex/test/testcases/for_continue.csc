var res = 1;
for(var i = 1; i < 10; ++i)
{
  continue;
  ++res;
}
return res == 1;