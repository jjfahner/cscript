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

#include "var.h"

class Socket : public Variant::Resource 
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
  Variant Connect(Variant const& host, Variant const& port);

  //
  // Close connection
  //
  Variant Disconnect();

  //
  // Send data
  //
  Variant Send(Variant const& data, Variant const& len = 0);

  //
  // Receive data
  //
  Variant Receive(Variant const& len, Variant const& block = true);


private:

  //
  // Member data
  //
  unsigned int m_socket;

};

#endif // CSCRIPT_SOCKET_H
