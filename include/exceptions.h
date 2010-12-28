//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_EXCEPTIONS_H
#define CSCRIPT_EXCEPTIONS_H

//
// Base class for all script exceptions
//
struct ScriptException : public std::runtime_error
{
  Object* m_node;
  ScriptException(Object* node, char const* message = "") : std::runtime_error(message), m_node (node) {}
  ScriptException(Object* node, String const& message) : std::runtime_error(message.c_str()), m_node (node) {}
  ~ScriptException() throw () {}
};

//
// Exceptions catchable from script
//
struct CatchableException : public ScriptException
{
  Value m_value;
  CatchableException(Value value) : ScriptException(0), m_value (value) {}
  CatchableException(Object* node) : ScriptException (node) {}
  CatchableException(Object* node, Value value) : ScriptException (node), m_value (value) {}
  ~CatchableException() throw() {}
};

struct UserException : public CatchableException
{
  UserException(Object* node) : CatchableException (node) {}
  UserException(Object* node, Value value) : CatchableException (node, value) {}
  ~UserException() throw() {}
};

//
// Easily define an exception
//
#define DEF_EXCEPTION(name, text) \
struct name : public CatchableException \
{ \
  name () : CatchableException(text) {} \
}

#endif // CSCRIPT_EXCEPTIONS_H
