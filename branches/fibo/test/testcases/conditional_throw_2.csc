var r;
try {
  r = false ? throw 0 : 1;
}
catch(e) {
  r = e;
}
return r;