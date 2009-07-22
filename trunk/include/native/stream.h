//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_STREAM_H
#define CSCRIPT_STREAM_H

#include <cscript.h>
#include <object.h>
#include <native.h>
#include <exceptions.h>

DEF_EXCEPTION(StreamAtEof,      "Stream at eof");
DEF_EXCEPTION(StreamNotOpen,    "Stream not open");
DEF_EXCEPTION(StreamBadState,   "Stream in error state");
DEF_EXCEPTION(StreamReadFail,   "Stream could not be read");
DEF_EXCEPTION(StreamWriteFail,  "Stream could not be written");


//
// Input stream
//
class IStream
{
public:

  //
  // Read a character
  //
  virtual Value ReadChar() = 0;

  //
  // Read the next whitespace-delimited string
  //
  virtual Value ReadString() = 0;

  //
  // Read the next line
  //
  virtual Value ReadLn() = 0;

};

//
// Output stream
//
class OStream
{
public:

  //
  // Write a character
  //
  virtual void WriteChar(ValueCRef value) = 0;

  //
  // Write supplied arguments to output
  //
  virtual void Write(ArgsCRef args) = 0;

  //
  // Write supplied arguments to output with automatic line feed
  //
  virtual void WriteLn(ArgsCRef args) = 0;

};

//
// Combined I/O stream
//
class IOStream : public IStream, public OStream
{
public:

};

#endif // CSCRIPT_STREAM_H
