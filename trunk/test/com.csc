// Create excel instance
var e = cocreate("Excel.Application");

// Add sheet activation events
e.SheetActivate = function(sheet) { 
  print("SheetActivate\n");
  sheet.Change = function(target) {
    print("Range: " + target.text + "\n");
  };
};

// Create a new workbook
e.Workbooks.Add();

// Show excel
e.visible = true;

// Quit
e.Quit();

// Release excel
e = null;

// Free memory
collect();

// Sleep for a while
cosleep(10000);
