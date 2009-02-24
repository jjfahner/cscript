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
#include <timer.h>

#if defined(WIN32)
#include <windows.h>
#elif defined(__GNUC__)
#include <sys/time.h>
#else
#error Not implemented on this platform
#endif

Timer::Timer(bool autoStart) :
m_start (0)
{
  if(autoStart)
  {
    Reset();
  }
}

uint64 
Timer::Elapsed() const
{
  return Ticks() - m_start;
}

void 
Timer::Reset()
{
  m_start = Ticks();
}

/*static*/ uint64 
Timer::Ticks()
{
# ifdef WIN32
  return GetTickCount();
# else
  timeval t;
  gettimeofday(&t, 0);
  return (t.tv_sec * 1000000 + t.tv_usec) / 1000;
# endif
}
