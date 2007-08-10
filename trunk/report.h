#ifndef CSCRIPT_REPORT_H
#define CSCRIPT_REPORT_H

#include "types.h"

#define STRINGIFY(a) #a
#define TOSTRING(a) STRINGIFY(a)
#define INTERNAL_ERROR(reporter, pos) \
  { \
    reporter.ReportError(pos, "internal compiler error on line " TOSTRING(__LINE__) " in file " __FILE__); \
  }

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
