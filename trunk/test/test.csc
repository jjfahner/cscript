//
// Create base prototype
//
var Foo = {
  m_foo : "foo",
  m_val : "foo",
  Foo : function() { return m_foo + ":" + m_val; }
};

//
// Create prototype with Foo as protype
//
var Bar = {
  prototype : Foo,
  m_bar : "bar",
  m_val : "bar",
  Bar : function() { return m_bar + ":" + m_val; }
};

// Create instance of bar
var inst = new Bar();

//
// Attach new prototype as base of foo
//
Foo.prototype = {
  m_baz : "baz",
  m_val : "baz",
  Baz : function() { return m_baz + ":" + m_val; }
};

// Test methods
print(inst.Foo() + "\n");
print(inst.Bar() + "\n");
print(inst.Baz() + "\n");
