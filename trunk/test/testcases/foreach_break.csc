var r = 0;
for(var i in [0, 1, 2, 3])
{
  if(i > 2)
  {
    break;
  }
  ++r;
}
return r == 3;