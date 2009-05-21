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

class Evaluator;
class Arguments;

class DataType : public Object
{
public:

  //
  // Type has member
  //
  bool ContainsKey(String const& key, bool checkProto) const;

  //
  // Find member
  //
  bool Find(String const& key, RValue*& pValue, bool checkProto) const;

  //
  // Type name as string
  //
  virtual String TypeName() = 0;

};

//////////////////////////////////////////////////////////////////////////

class VoidType : public DataType
{
public:

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

  //
  // Type name as string
  //
  virtual String TypeName();

};

//////////////////////////////////////////////////////////////////////////

class UnknownType : public DataType
{
public:

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

  //
  // Type name as string
  //
  virtual String TypeName();

};

//////////////////////////////////////////////////////////////////////////

class ScalarType : public DataType
{
public:

  //
  // Type has member
  //
  bool ContainsKey(String const& key, bool checkProto) const;

  //
  // Find member
  //
  bool Find(String const& key, RValue*& pValue, bool checkProto) const;

  //
  // Convert to string
  //
  virtual String ToString() = 0;

  //
  // Box a scalar value
  //
  virtual DataType* Box(Value const& value) = 0;

};

class NullType : public ScalarType
{
public:

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

  //
  // Type name as string
  //
  virtual String TypeName();

  //
  // Convert to string
  //
  virtual String ToString();

  //
  // Box value
  //
  virtual DataType* Box(Value const&);

};

//////////////////////////////////////////////////////////////////////////

class BooleanType : public ScalarType
{
public:

  typedef bool DeclType;

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

  //
  // Constructor
  //
  BooleanType(DeclType value = DeclType());

  //
  // Type name as string
  //
  virtual String TypeName();

  //
  // Convert to string
  //
  virtual String ToString();

  //
  // Box value
  //
  virtual BooleanType* Box(Value const& value);

protected:

  DeclType m_value;

};

//////////////////////////////////////////////////////////////////////////

class IntegerType : public ScalarType
{
public:

  typedef __int64 DeclType;

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

  //
  // Constructor
  //
  IntegerType(DeclType value = DeclType());

  //
  // Type name as string
  //
  virtual String TypeName();

  //
  // Convert to string
  //
  virtual String ToString();

  //
  // Box value
  //
  virtual IntegerType* Box(Value const& value);
 
protected:

  DeclType m_value;

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

  //
  // Constructor
  //
  StringType(DeclType const& value = DeclType());

  //
  // Type name as string
  //
  virtual String TypeName();

  //
  // Convert to string
  //
  virtual String ToString();

  //
  // Box value
  //
  virtual StringType* Box(Value const& value);

protected:

  DeclType m_value;

};

//////////////////////////////////////////////////////////////////////////

class ComplexType : public DataType
{
};

//////////////////////////////////////////////////////////////////////////

class ObjectType : public ComplexType
{
public:

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

  //
  // Type name as string
  //
  virtual String TypeName();

};

//////////////////////////////////////////////////////////////////////////

class FunctionType : public ObjectType
{
public:

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

  //
  // Type name as string
  //
  virtual String TypeName();

};

//////////////////////////////////////////////////////////////////////////

class NativeFunctionType : public FunctionType
{
public:

  //
  // Retrieve singleton instance
  //
  static DataType* Instance();

  //
  // Type name as string
  //
  virtual String TypeName();

};

#endif // CSCRIPT_DATATYPE_H
