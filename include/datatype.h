//////////////////////////////////////////////////////////////////////////
//
// This file is � 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include <native.h>

class Evaluator;
class Arguments;

//////////////////////////////////////////////////////////////////////////

class DataType : public Object
{
public:

  //
  // Native calls
  //
  DEF_NATIVE_CALLS(DataType, Object);

  //
  // Data type
  //
  virtual DataType* GetType();

  //
  // Type name as string
  //
  __native_roprop virtual String TypeName() = 0;

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
  DEF_NATIVE_CALLS(ScalarType, DataType)

  //
  // Convert to string
  //
  __native_method virtual String ToString() = 0;

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
  DEF_NATIVE_CALLS(IntegerType, ScalarType)

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

  //
  // Parse an integer
  //
  __native_method virtual int64 ParseInt(StringCRef source) = 0;

  //
  // Operators
  //
  __native_method virtual int64 Add(int64 rhs) = 0;
  __native_method virtual int64 Sub(int64 rhs) = 0;
  __native_method virtual int64 Mul(int64 rhs) = 0;
  __native_method virtual int64 Div(int64 rhs) = 0;
  __native_method virtual int64 Mod(int64 rhs) = 0;

};

//////////////////////////////////////////////////////////////////////////

class RealType : public ScalarType
{
public:

  //
  // Native calls
  //
  DEF_NATIVE_CALLS(IntegerType, ScalarType)

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

};

class StringType : public ScalarType
{
public:

  DEF_NATIVE_CALLS(StringType, ScalarType);

  //
  // Type declaration
  //
  typedef ::String DeclType;

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

  //
  // Length of string
  //
  __native_roprop virtual int64 Length() = 0;

  //
  // Binary operators
  //
  __native_method virtual String Add(ValueCRef rhs) = 0;

  //
  // Retrieve substring
  //
  __native_method virtual String Substr(int64 start, int64 length = 0) = 0;

  //
  // Find offset of substring
  //
  __native_method virtual int64 Find(StringCRef what, int64 start = 0) = 0;

  //
  // Split a string into pieces
  //
  __native_method virtual Value Split(ValueCRef sep = "\x32") = 0;

  //
  // Replace all occurrences of a substring
  //
  __native_method virtual Value Replace(StringCRef what, StringCRef with) = 0;

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
