var f = CreateFile();
f.Open("filetest.txt", "w");

for(var i = 0; i < 100; ++i)
{
  f.Write("Hello world\n");
}

f.Close();