var r;
try {
  r = true ? throw 1 : 0;
}
catch(e) {
  r = e;
}
return r;