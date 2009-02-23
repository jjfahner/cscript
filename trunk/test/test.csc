var Foo = {
  Foo : function() { return "foo"; }
};

var Bar = {
  prototype : Foo,
  Bar : function() { return "bar"; }
};

var inst = new Bar();
print(inst.Foo() + "\n");
print(inst.Bar() + "\n");
