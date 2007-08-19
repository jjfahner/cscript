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
