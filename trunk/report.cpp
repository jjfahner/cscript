#include "report.h"

Reporter::Reporter() :
m_errors  (0),
m_warnings(0)
{
}

void 
Reporter::ReportError(FilePos& pos, String const& text)
{
  ++m_errors;
  std::cout << pos.m_file << " (" << pos.m_line << ") : error: " << text << std::endl;
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
