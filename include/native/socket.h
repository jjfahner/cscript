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
#ifndef CSCRIPT_SOCKET_H
#define CSCRIPT_SOCKET_H

#include <native.h>
#include <object.h>

class Socket : public Object
{
public:

  DEF_NATIVE_CALLS(Socket, Object);

  //
  // Construction
  //
  Socket();

  //
  // Destruction
  //
  virtual ~Socket();

  //
  // Connect to peer
  //
  __native_method Value Connect(StringCRef host, int64 port);

  //
  // Close connection
  //
  __native_method Value Disconnect();

  //
  // Send data
  //
  __native_method Value Send(StringCRef data, int64 length = 0);

  //
  // Receive data
  //
  __native_method Value Receive(int64 length, int64 timeout = 0);


private:

  //
  // Member data
  //
  unsigned int m_socket;

};

#endif // CSCRIPT_SOCKET_H
