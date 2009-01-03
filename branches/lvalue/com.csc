var con = cocreate("ADODB.Connection.2.8");
print("Before: " + con.ConnectionString + "\n");
con.ConnectionString = "db=test";
print("After: " + con.ConnectionString + "\n");
con.Open("db=test", "testuser", "testpwd");
