#ifndef CSCRIPT_REPORT_H
#define CSCRIPT_REPORT_H

class Reporter 
{
public:

  //
  // Report an error
  //
  virtual void ReportError(String const& text) = 0;

  //
  // Report a warning
  //
  virtual void ReportWarning(String const& text) = 0;

};

#endif // CSCRIPT_REPORT_H
