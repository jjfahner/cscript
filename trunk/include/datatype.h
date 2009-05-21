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
  bool ContainsKey(Value const& key, bool checkProto) const;

  //
  // Find member
  //
  bool Find(Value const& key, RValue*& pValue, bool checkProto) const;

  //
  // Type name as string
  //
  virtual String TypeName() const = 0;

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
  virtual String TypeName() const;

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
  virtual String TypeName() const;

};

//////////////////////////////////////////////////////////////////////////

class ScalarType : public DataType
{
public:

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
  virtual String TypeName() const;

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
  virtual String TypeName() const;

  //
  // Convert to string
  //
  virtual String ToString() const;

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
  virtual String TypeName() const;

  //
  // Convert to string
  //
  virtual String ToString() const;

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
  virtual String TypeName() const;

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
  virtual String TypeName() const;

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
  virtual String TypeName() const;

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
  virtual String TypeName() const;

};

#endif // CSCRIPT_DATATYPE_H
