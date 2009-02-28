//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include <report.h>
#include <stdio.h>
#include <stdarg.h>
#include <iostream>

Reporter::Reporter() :
m_errors  (0),
m_warnings(0)
{
}

void 
Reporter::Reset()
{
  m_errors   = 0;
  m_warnings = 0;
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
  std::cout << pos.m_file << " (" << pos.m_line << ") : warning: " << text << "\n";
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
