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
#ifndef CSCRIPT_SOCKET_H
#define CSCRIPT_SOCKET_H

#include "value.h"
#include "object.h"

class Socket : public Object
{
public:

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
  Value Connect(Value const& host, Value const& port);

  //
  // Close connection
  //
  Value Disconnect();

  //
  // Send data
  //
  Value Send(Value const& data, Value const& len = 0);

  //
  // Receive data
  //
  Value Receive(Value const& len, Value const& timeout = Value());


private:

  //
  // Member data
  //
  unsigned int m_socket;

};

#endif // CSCRIPT_SOCKET_H
