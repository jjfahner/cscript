var e = cocreate("Excel.Application");

e.visible = true;
e.SheetActivate = function(sheet) { sheet.Close(); };

cosleep(10000);