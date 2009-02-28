var Foo = {

  m_value : 1,

  operator == : function(int rhs) {
    return m_value == rhs;
  }
};

var f = new Foo;
print(f == 1);