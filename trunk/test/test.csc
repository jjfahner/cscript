
var Integer = [
  
  // Value
  "m_value"   : 0,

  // Addition
  "operator+" : function(int rhs) {
    var res = new Integer;
    res.m_value = m_value + rhs;
    return res;
  },
  
  // Subtraction
  "operator-" : function(int rhs) {
    var res = new Integer;
    res.m_value = m_value - rhs;
    return res;
  },

  // Implicit conversion to bool
  "operator bool" : function() {
    return m_value != 0;
  },

  // Implicit conversion to integer
  "operator int" : function() {
    return m_value;
  },

  // Implicit conversion to string
  "operator string" : function() {
    return string(m_value);
  }

];

var a = new Integer;
a = a + 2;
a = a + a;
print(string(a));
