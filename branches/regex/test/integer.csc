//
// Declare integer class
//
var Integer = {
  
  // Integer value
  m_value : 0,

  // Constructor
  constructor : function(int value = 0) {
    m_value = value;
  },

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
  },

  // Function call operator
  operator () : function(int a, int b) {
    return a * b * m_value;
  }

};

var a = new Integer(10);

// Test addition and substraction
a = a + 2 - 1;

// Test assignment operators
a += a += 3;

// Invoke string conversion
print(string(a) + "\n");

// Invoke function call operator
print(string(a(3,4)) + "\n");
