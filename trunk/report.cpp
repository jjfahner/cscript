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

  // Calculate buffer size
  int len = _scprintf(szTemplate, 
    pos->m_file.c_str(), pos->m_line, 
    notice.m_number, notice.m_notice);

  // Allocate and fill template string
  char* pszTemplate = new char[len + 1];
  sprintf(pszTemplate, szTemplate, 
    pos->m_file.c_str(), pos->m_line, 
    notice.m_number, notice.m_notice);

  // Prepare arglist
  va_list args;
  va_start(args, pos);

  // Calculate buffer size
  len = _vscprintf(pszTemplate, args);

  // Allocate and fill final string
  char* pszFinal = new char[len + 1];
  vsprintf(pszFinal, pszTemplate, args);

  // Write string
  std::cout << pszFinal;

  // Free strings
  delete [] pszTemplate;
  delete [] pszFinal;
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
