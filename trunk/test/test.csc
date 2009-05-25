function assert(a,b,c) {
}

function foo()
{
  var a;
  a = [];
  a[] = 0;
  a[] = 1;
  a[] = 2;
  assert("Array entries", count(a), 3);
  assert("Array entries", a[0], 0);
  assert("Array entries", a[1], 1);
  assert("Array entries", a[2], 2);

  for(a = 0; a < 10; ++a) {}
}

foo();
