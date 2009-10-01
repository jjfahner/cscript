
function wndproc(hwnd, message, wparam, lparam)
{
  switch(message)
  {
  case 0x0081:
    return 1;
  case 0x0010:
    Winapi.User32.DestroyWindow(hwnd);
    Winapi.User32.PostQuitMessage(0);
    return 0;
  default:     
    return Winapi.User32.DefWindowProcA(hwnd, message, wparam, lparam);
  }
}

var hModule = Winapi.Kernel32.GetModuleHandleA(0);

var wndclass = [
  0, // style
  wndproc,
  0, // classExtra
  0, // wndExtra
  hModule,
  Winapi.User32.LoadIconA(0, 32512),
  Winapi.User32.LoadCursorA(0, 32512),
  Winapi.Gdi32.CreateSolidBrush(Winapi.User32.GetSysColor(15)),
  0, // Menu name
  "CScriptWindowClass"
];

Console.WriteLn(wndclass);

if (Winapi.User32.RegisterClassA(wndclass) == 0)
{
  Console.WriteLn("Failed to register class");
  return;
}

Console.WriteLn("Registered class");

var hwnd = Winapi.User32.CreateWindowExA(
  0, "CScriptWindowClass", "Foobar", 0x10cf0000, 0, 0, 100, 100, 0, 0, hModule, 0);
if (hwnd == 0)
{
  Console.WriteLn("Window creation failed");
  return;
}

Console.WriteLn("Created window with handle ", hwnd);

var msg = [ 0, 0, 0, 0, 0, [ 0, 0 ]];
while(Winapi.User32.GetMessageA(msg, 0, 0, 0))
{
  Winapi.User32.TranslateMessage(msg);
  Winapi.User32.DispatchMessageA(msg);
}
