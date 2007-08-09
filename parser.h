#ifndef CSCRIPT_PARSE_H
#define CSCRIPT_PARSE_H

#include "types.h"

class File;

class Parser
{
public:

  //
  // Construction
  //
  Parser();

  //
  // Destruction
  //
  ~Parser();

  //
  // Parse a file
  //
  void Parse(File& file);
  void Parse(String const& filename);

  //
  // Root node
  //
  Ast* GetRoot() const;
  void SetRoot(Ast* root);

  //
  // Error handlers
  //
  void OnParseFailure();
  void OnSyntaxError();

private:

  //
  // Members
  //
  Ast* m_root;

};

#endif // CSCRIPT_PARSE_H
