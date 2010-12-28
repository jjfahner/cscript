//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_REPORT_H
#define CSCRIPT_REPORT_H

#include <cscript.h>

class Reporter 
{
public:

  //
  // Construction
  //
  Reporter();

  //
  // Reset
  //
  void Reset();

  //
  // Report a warning
  //
  void ReportWarning(String const& text);

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
  // MemberMap
  //
  int m_errors;
  int m_warnings;

};

#endif // CSCRIPT_REPORT_H
