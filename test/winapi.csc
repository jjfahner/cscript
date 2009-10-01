
function EnumWindowProc(hwnd, param)
{
  var len = Winapi.User32.GetWindowTextLengthA(hwnd);
  if(len > 0)
  {
    var title = Winapi.StringBuf(len + 1);
    Winapi.User32.GetWindowTextA(hwnd, title, len + 1);
    Console.WriteLn("Window: ", hwnd, " ", title.ToString());
  }
  return 1;
}

Winapi.User32.EnumWindows(EnumWindowProc, 0);

var Window =
{
  m_hwnd: 0,

  Create: function() {

    // Register the window class
    RegisterClass();

    // Create the window
    this.m_hwnd = Winapi.User32.CreateWindowExA(0,
        "CScriptWindowClass", "Foobar",
        0x10cf0000, 0, 0, 100, 100, 0,
        0, hModule, 0);

    // Check creation
    if (this.m_hwnd == 0) {
      throw "Creation failed";
    }
  },

  RegisterClass: function() {
    var wndclass = [
      0, // style
      WndProc,
      0, // classExtra
      0, // wndExtra
      hModule,
      Winapi.User32.LoadIconA(0, 32512),
      Winapi.User32.LoadCursorA(0, 32512),
      Winapi.Gdi32.CreateSolidBrush(Winapi.User32.GetSysColor(15)),
      0, // Menu name
      "CScriptWindowClass"
    ];

    if (Winapi.User32.RegisterClassA(wndclass) == 0) {
      throw "Create failed";
    }
  },

  WndProc: function(hwnd, message, wparam, lparam) {
    switch (message) {
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

};

var hModule = Winapi.Kernel32.GetModuleHandleA(0);

var wnd = new Window;
wnd.Create();

var msg = [ 0, 0, 0, 0, 0, [ 0, 0 ]];
while(Winapi.User32.GetMessageA(msg, 0, 0, 0))
{
  Winapi.User32.TranslateMessage(msg);
  Winapi.User32.DispatchMessageA(msg);
}

Console.WriteLn("Exited message loop");
