#include "socket.h"
#include "native.h"

// Include windows socket headers
#ifdef _MSC_VER
#	include <winsock2.h>
#	include <ws2tcpip.h>
#	pragma comment(lib, "ws2_32.lib")
#else
#	include <netdb.h>
#	define INVALID_SOCKET (-1)
#	define SD_BOTH SHUT_RDWR
#	define SOCKET_ERROR (-1)
#	define closesocket close
#endif

static int g_numSockets = 0;

void SocketCreate()
{
  if(++g_numSockets == 1)
  {
#ifdef MSC_VER
    WSADATA wd;
    WSAStartup(MAKEWORD(2, 0), &wd);
#endif
  }
}

void SocketDelete()
{
  if(--g_numSockets == 0)
  {
#ifdef MSC_VER
    WSACleanup();
#endif
  }
}

Socket::Socket() :
m_socket (INVALID_SOCKET)
{
  // Global initialization
  SocketCreate();
}

Socket::~Socket()
{
  // Close connection
  Disconnect();

  // Global cleanup
  SocketDelete();
}

Variant
Socket::Connect(Variant const& host, Variant const& port)
{
  // Disconnect previous connection
  if(m_socket != INVALID_SOCKET)
  {
    Disconnect();
  }

  // Retrieve info
  addrinfo* ai;
  if(getaddrinfo(Variant(host).AsString().c_str(), 
                 Variant(port).AsString().c_str(), 
                 0, &ai))
  {
    return Variant::False;
  }

  // Create a socket
  unsigned int s = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
  if(s == INVALID_SOCKET)
  {
    return Variant::False;
  }

  // Establish connection
  if(connect(s, (sockaddr*)ai->ai_addr, sizeof(sockaddr)))
  {
    closesocket(s);
    return Variant::False;
  }

  // Store socket handle
  m_socket = s;

  // Succeeded
  return Variant::True;
}

Variant 
Socket::Disconnect()
{
  if(m_socket != INVALID_SOCKET)
  {
    shutdown(m_socket, SD_BOTH);
    closesocket(m_socket);
    m_socket = INVALID_SOCKET;
  }
  return Variant::True;
}

Variant
Socket::Send(Variant const& data, Variant const& len)
{
  // Check connection
  if(m_socket == INVALID_SOCKET)
  {
    return Variant::False;
  }

  // Determine length
  int expected = (int)len.AsInt();
  if(expected == 0)
  {
    expected = data.AsString().length();
  }

  // Write data
  int actual = send(m_socket, data.AsString().c_str(), expected, 0);
  if(actual == SOCKET_ERROR)
  {
    return Variant::False;
  }

  // Return result
  return actual == expected ? Variant::True : Variant::False;
}

Variant 
Socket::Receive(Variant const& length, Variant const& block)
{
  // Check connection
  if(m_socket == INVALID_SOCKET)
  {
    return Variant::False;
  }

  // Allocate buffer
  int len = int(length.AsInt());
  char* buf = new char[len + 1];

  // Read requested bytes
  int read = recv(m_socket, buf, len, 0);
  if(read == SOCKET_ERROR)
  {
    delete [] buf;
    return Variant::False;
  }

  // Copy to string
  buf[read] = 0;
  String result(buf);

  // Free buffer
  delete [] buf;

  // Return buffer
  return result;
}

//////////////////////////////////////////////////////////////////////////
//
// Native call interface to socket class
//

NATIVE_CALL(socket, 2, 2)
{
  ASSERT_TYPE(0, stString);
  ASSERT_TYPE(1, stInt);

  Socket* s = new Socket();
  if(!s->Connect(*args[0], *args[1]))
  {
    delete s;
    return Variant::False;
  }

  return Variant(s);
}

NATIVE_CALL(closesocket, 1, 1)
{
  Socket* s = args[0]->GetTypedRes<Socket>();
  s->Disconnect();
  return Variant::True;
}

NATIVE_CALL(send, 2, 3)
{
  Socket* s = args[0]->GetTypedRes<Socket>();
  if(args.size() == 2)
  {
    return s->Send(*args[1]);
  }
  else
  {
    return s->Send(*args[1], *args[2]);
  }
}

NATIVE_CALL(recv, 2, 2)
{
  Socket* s = args[0]->GetTypedRes<Socket>();
  return s->Receive(*args[1]);  
}

