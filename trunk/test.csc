var a = [
  "foo" : function() { print("In foo:{baz}\n"); },
  "bar" : function() { print("In bar:{baz}\n"); },
  "baz" : 10
];

a.foo();
a.bar();