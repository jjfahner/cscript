#ifndef CSCRIPT_VALUEMAP_H
#define CSCRIPT_VALUEMAP_H

#include <map>

class Value;
class Evaluator;

//
// Less-than comparator
//
class ValueComparatorLess
{
public:

  //
  // Construction
  //
  ValueComparatorLess(Evaluator* evaluator);

  //
  // Less-than operator
  //
  bool operator () (Value const& lhs, Value const& rhs) const;

private:

  Evaluator* m_evaluator;

};

//
// Value map
//
typedef std::map<Value, Value, ValueComparatorLess> ValueMap;

#endif // CSCRIPT_VALUEMAP_H
