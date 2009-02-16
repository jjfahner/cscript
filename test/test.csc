var s = CreateSocket();
s.Connect("jan-jaap.net", 80);
s.Send("GET / HTTP/1.0\r\nHost: jan-jaap.net\r\nUser-Agent: cscript\r\n\r\n");
while(true)
{
  var r = s.Receive(1024, 1);
  if(r == null)
  {
    break;
  }
  print(r);
}

s.Disconnect();
