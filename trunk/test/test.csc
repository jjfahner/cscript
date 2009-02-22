//
// Declare integer class
//
var Integer = {
  
  // Initial value
  m_value : 0,

  // Assignment
  operator = : function(int rhs) {
    m_value = rhs;
    return this;
  },

  // Addition
  operator + : function(int rhs) {
    var res = new Integer;
    res.m_value = m_value + rhs;
    return res;
  },
  
  // Addition
  operator += : function(int rhs) {
    m_value += rhs;
    return this;
  },

  // Subtraction
  operator - : function(int rhs) {
    var res = new Integer;
    res.m_value = m_value - rhs;
    return res;
  },

  // Subtraction
  operator -= : function(int rhs) {
    m_value -= rhs;
    return this;
  },

  // Implicit conversion to bool
  operator bool : function() {
    return m_value != 0;
  },

  // Implicit conversion to integer
  operator int : function() {
    return m_value;
  },

  // Implicit conversion to string
  operator string : function() {
    return string(m_value);
  }

};

var a = new Integer;
a += a += 3;
a = a + 2 - 1;
a = a + a;
print(string(a));
