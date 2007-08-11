#include "report.h"
#include <stdio.h>
#include <stdarg.h>


Reporter::Reporter() :
m_errors  (0),
m_warnings(0)
{
}

void 
Reporter::ReportError(Notice const& notice, FilePos* pos, ...)
{
  static const char* szTemplate = "%s (%d) : error E%04d: %s\n";

  // Update error count
  ++m_errors;

  // Build first generation from template
  char buf1[4096];
  sprintf(buf1, szTemplate, 
    pos->m_file.c_str(), pos->m_line, 
    notice.m_number, notice.m_notice);

  // Prepare arglist
  va_list args;
  va_start(args, pos);

  // Build second generation from first and variadic args
  char buf2[8192];
  vsprintf(buf2, buf1, args);

  // Write string
  std::cout << buf2;
}

void 
Reporter::ReportWarning(FilePos& pos, String const& text)
{
  ++m_warnings;
  std::cout << pos.m_file << " (" << pos.m_line << ") : warning: " << text << std::endl;
}

int 
Reporter::GetErrorCount() const
{
  return m_errors;
}

int 
Reporter::GetWarningCount() const
{
  return m_warnings;
}
