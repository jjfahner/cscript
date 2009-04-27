var e = cocreate("Excel.Application");

e.visible = true;
e.SheetActivate = function(sheet) { 
  sheet.Change = function(target) {
    print("Range: " + target.text + "\n");
  };
};

cosleep(30000);