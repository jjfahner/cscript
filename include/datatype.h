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
#ifndef CSCRIPT_DATATYPE_H
#define CSCRIPT_DATATYPE_H

#include <cscript.h>
#include <object.h>
#include <stubs.h>

class Evaluator;
class Arguments;

//////////////////////////////////////////////////////////////////////////

class DataType : public Object
{
public:

  //
  // Native calls
  //
  IMPL_NATIVECALLS(DataType, Object)

  //
  // Construction
  //
  DataType();

  //
  // Type name as string
  //
  __native virtual String TypeName() = 0;

};

//////////////////////////////////////////////////////////////////////////

class VoidType : public DataType
{
public:

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

};

//////////////////////////////////////////////////////////////////////////

class UnknownType : public DataType
{
public:

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

};

//////////////////////////////////////////////////////////////////////////

class ScalarType : public DataType
{
public:

  //
  // Native calls
  //
  IMPL_NATIVECALLS(ScalarType, DataType)

  //
  // Convert to string
  //
  __native virtual String ToString() = 0;

  //
  // Box a scalar value
  //
  virtual DataType* Box(Value const& value) = 0;

};

//////////////////////////////////////////////////////////////////////////

class NullType : public ScalarType
{
public:

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

};

//////////////////////////////////////////////////////////////////////////

class BooleanType : public ScalarType
{
public:

  //
  // Declaration type
  //
  typedef bool DeclType;

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

};

//////////////////////////////////////////////////////////////////////////

class IntegerType : public ScalarType
{
public:

  //
  // Native calls
  //
  IMPL_NATIVECALLS(IntegerType, ScalarType)

  //
  // Declaration type
  //
  typedef int64 DeclType;

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

  //
  // Parse an integer
  //
  __native virtual DeclType ParseInt(String source) = 0;

};

//////////////////////////////////////////////////////////////////////////

class StringType : public ScalarType
{
public:

  //
  // Type declaration
  //
  typedef ::String DeclType;

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

};

//////////////////////////////////////////////////////////////////////////

class CompoundType : public DataType
{
};

//////////////////////////////////////////////////////////////////////////

class ObjectType : public CompoundType
{
public:

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

};

//////////////////////////////////////////////////////////////////////////

class FunctionType : public ObjectType
{
public:

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

};

//////////////////////////////////////////////////////////////////////////

class NativeFunctionType : public FunctionType
{
public:

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

};

#endif // CSCRIPT_DATATYPE_H
