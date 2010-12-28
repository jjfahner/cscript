//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_LEMON_PARSER_H
#define CSCRIPT_LEMON_PARSER_H

#include <cstdlib>

template <
  typename ParserClass, 
  typename TokenClass,
  void* pfunParserAlloc(void *(*)(size_t)),
  void  pfunParserFree(void *, void (*)(void*)),
  void  pfunParserTrace(FILE *, char *),
  void  pfunParse(void *yyp, int, TokenClass, ParserClass*)
>
class LemonParser
{
public:

  //
  // Allocate parser
  //
  LemonParser(ParserClass* pParser, char const* prefix = "Parser: ", bool debug = false) :
  m_pParser (pParser),
  m_hParser (0)
  {
    m_hParser = pfunParserAlloc(malloc);
    if(debug)
    {
#ifdef _DEBUG
      pfunParserTrace(stdout, (char*)prefix);
#else
      throw std::runtime_error("Cannot debug parser in release build");
#endif
    }
  }

  //
  // Free parser
  //
  ~LemonParser()
  {
    if(m_hParser)
    {
      pfunParserFree(m_hParser, free);
    }
  }

  //
  // Parse next token
  //
  void operator () (int type, TokenClass token)
  {
    pfunParse(m_hParser, type, token, m_pParser);
  }

  //
  // Flush parser
  //
  void operator () ()
  {
    pfunParse(m_hParser, 0, TokenClass(), m_pParser);
  }

private:

  //
  // Members
  //
  ParserClass*  m_pParser;
  void*         m_hParser;

};

#endif // CSCRIPT_LEMON_PARSER_H
