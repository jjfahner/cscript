#ifndef CSCRIPT_REPORT_H
#define CSCRIPT_REPORT_H

#include "types.h"
#include "error.h"

#define INTERNAL_ERROR(reporter, pos) \
  { \
    reporter.ReportError(E0001, &(pos), __FILE__, __LINE__); \
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
  void ReportError(Notice const& notice, FilePos* pos, ...);

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
