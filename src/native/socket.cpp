//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "native/socket.h"
#include "eval.h"

DEFINE_NATIVE_LINKAGE(Socket)

// Include windows socket headers
#ifdef _MSC_VER
#	include <winsock2.h>
#	include <ws2tcpip.h>
#	pragma comment(lib, "ws2_32.lib")
# undef GetObject
#else
#	include <netdb.h>
#	define INVALID_SOCKET (-1)
#	define SD_BOTH SHUT_RDWR
#	define SOCKET_ERROR (-1)
#	define closesocket close
#endif

//////////////////////////////////////////////////////////////////////////

class Socket : public Object
{
public:

  //
  // Construction
  //
  Socket(Evaluator* eval);

  //
  // Destruction
  //
  virtual ~Socket();

  //
  // Connect to peer
  //
  Value Connect(Evaluator*, Arguments const&);

  //
  // Close connection
  //
  Value Disconnect(Evaluator*, Arguments const&);

  //
  // Send data
  //
  Value Send(Evaluator*, Arguments const&);

  //
  // Receive data
  //
  Value Receive(Evaluator*, Arguments const&);


private:

  //
  // Member data
  //
  unsigned int m_socket;

};

//////////////////////////////////////////////////////////////////////////

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

//////////////////////////////////////////////////////////////////////////

Socket::Socket(Evaluator* evaluator) :
m_socket (INVALID_SOCKET)
{
  // Global initialization
  SocketCreate();

  // Register methods
  NATIVE_METHOD(Socket, Connect,    "Connect(string host, int port)");
  NATIVE_METHOD(Socket, Disconnect, "Disconnect()");
  NATIVE_METHOD(Socket, Send,       "Send(string data, int len = 0)");
  NATIVE_METHOD(Socket, Receive,    "Receive(int len, int timeout = 0)");
}

Socket::~Socket()
{
  // Close connection
  Disconnect(0, Arguments());

  // Global cleanup
  SocketDelete();
}

Value
Socket::Connect(Evaluator*, Arguments const& args)
{ 
  // Extract parameters
  String host = args[0].GetString();
  String port = ValString(args[1].GetInt());

  // Disconnect previous connection
  if(m_socket != INVALID_SOCKET)
  {
    Disconnect(0, Arguments());
  }

  // Create hint
  addrinfo hint;
  memset(&hint, 0, sizeof(hint));
  hint.ai_family   = AF_INET;
  hint.ai_socktype = SOCK_STREAM;
  hint.ai_protocol = IPPROTO_TCP;

  // Retrieve info
  addrinfo* ai;
  if(getaddrinfo(host.c_str(), 
                 port.c_str(), 
                 &hint, &ai))
  {
    return false;
  }

  // Create a socket
  unsigned int s = (unsigned int)socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
  if(s == INVALID_SOCKET)
  {
    return false;
  }

  // Establish connection
  if(connect(s, (sockaddr*)ai->ai_addr, sizeof(sockaddr)))
  {
    closesocket(s);
    return false;
  }

  // Store socket handle
  m_socket = s;

  // Succeeded
  return true;
}

Value 
Socket::Disconnect(Evaluator*, Arguments const&)
{
  if(m_socket != INVALID_SOCKET)
  {
    shutdown(m_socket, SD_BOTH);
    closesocket(m_socket);
    m_socket = INVALID_SOCKET;
  }
  return true;
}

Value
Socket::Send(Evaluator*, Arguments const& args)
{
  String data = args[0].GetString();
  Value::Int len = args[1].GetInt();

  // Check connection
  if(m_socket == INVALID_SOCKET)
  {
    return false;
  }

  // Determine length
  int expected = (int)len;
  if(expected == 0)
  {
    expected = (int)data.length();
  }

  // Write data
  int actual = send(m_socket, data.c_str(), expected, 0);
  if(actual == SOCKET_ERROR)
  {
    return false;
  }

  // Return result
  return actual == expected ? true : false;
}

Value 
Socket::Receive(Evaluator*, Arguments const& args)
{
  Value::Int length  = args[0].GetInt();
  Value::Int timeout = args[1].GetInt();

  // Check connection
  if(m_socket == INVALID_SOCKET)
  {
    return Value();
  }

  // Determine timeout
  timeval  tv  = { 0, 0 };
  timeval* ptv = 0;
  if(timeout > 0)
  {
    ptv = &tv;
    ptv->tv_sec = (long)timeout;
  }

  // Make set of 1 sockets ;)
  fd_set fd;
  FD_ZERO(&fd);
  FD_SET(m_socket, &fd);

  // Check for readability
  if(select(1, &fd, 0, 0, ptv) != 1)
  {
    return Value();
  }

  // Allocate buffer
  int len = (int)length;
  char* buf = new char[len + 1];

  // Read requested bytes
  int read = recv(m_socket, buf, len, 0);

  // Connection closed by peer
  if(read == 0)
  {
    Disconnect(0, Arguments());
    return Value();
  }
  
  // Error
  if(read == SOCKET_ERROR)
  {
    delete [] buf;
    Disconnect(0, Arguments());
    return Value();
  }

  // Copy to string
  buf[read] = 0;
  String result(buf);

  // Free buffer
  delete [] buf;

  // Return buffer
  return result;
}

NATIVE_CALL("CreateSocket()")
{
  return new Socket(evaluator);
}
