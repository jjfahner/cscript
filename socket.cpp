//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
// This file is part of the cscript interpreter.
// CScript can be found at http://svn.jan-jaap.net/
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//////////////////////////////////////////////////////////////////////////
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
#ifdef _MSC_VER
    WSADATA wd;
    WSAStartup(MAKEWORD(2, 0), &wd);
#endif
  }
}

void SocketDelete()
{
  if(--g_numSockets == 0)
  {
#ifdef _MSC_VER
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

  // Create hint
  addrinfo hint;
  memset(&hint, 0, sizeof(hint));
  hint.ai_family   = AF_INET;
  hint.ai_socktype = SOCK_STREAM;
  hint.ai_protocol = IPPROTO_TCP;

  // Retrieve info
  addrinfo* ai;
  if(getaddrinfo(host.AsString().c_str(), 
                 port.AsString().c_str(), 
                 &hint, &ai))
  {
    return Variant::False;
  }

  // Create a socket
  unsigned int s = (unsigned int)socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
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
    expected = (int)data.AsString().length();
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
Socket::Receive(Variant const& length, Variant const& timeout)
{
  // Check connection
  if(m_socket == INVALID_SOCKET)
  {
    return Variant::False;
  }

  // Determine timeout
  timeval  tv  = { 0, 0 };
  timeval* ptv = 0;
  if(!timeout.Empty())
  {
    ptv = &tv;
    ptv->tv_sec = (long) timeout.GetInt();
  }

  // Make set of 1 sockets ;)
  fd_set fd;
  FD_ZERO(&fd);
  FD_SET(m_socket, &fd);

  // Check for readability
  if(select(FD_SETSIZE, &fd, 0, 0, ptv) != 1)
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

NATIVE_CALL(recv, 2, 3)
{
  Socket* s = args[0]->GetTypedRes<Socket>();
  if(args.size() == 2)
  {
    return s->Receive(*args[1]);  
  }
  else
  {
    return s->Receive(*args[1], *args[2]);  
  }
}

