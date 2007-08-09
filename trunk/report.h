#ifndef CSCRIPT_REPORT_H
#define CSCRIPT_REPORT_H

#include "types.h"

class Reporter 
{
public:

  //
  // Construction
  //
  Reporter();

  //
  // Report an error
  //
  void ReportError(FilePos& pos, String const& text);

  //
  // Report a warning
  //
  void ReportWarning(FilePos& pos, String const& text);

  //
  // Retrieve error count
  //
  int GetErrorCount() const;

  //
  // Retrieve warning count
  //
  int GetWarningCount() const;

private:

  //
  // Members
  //
  int m_errors;
  int m_warnings;

};

#endif // CSCRIPT_REPORT_H
