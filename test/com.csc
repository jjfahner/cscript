/////////////////////////////////////////////////////////////////////
//
// Test COM functionality
//

//
// Recursive descent through XML tree
//
function enumNodes(node)
{
  print(node.nodeName);
  
  if(node.attributes != null)
  {
	  for(var att in node.attributes)
	  {
		  print(" " + att.nodeName + "=" + att.nodeValue);	
	  }
  }

  print("\n");

  for(var elm in node.childNodes)
  {
	  enumNodes(elm);
  }
}

// Create document instance
var xmlDoc = cocreate("Microsoft.XMLDOM");

// Set properties
xmlDoc.async = false;
xmlDoc.preserveWhitespace = false;

// Load XML data
xmlDoc.load("sample.xml");

// Enumerate tree
enumNodes(xmlDoc);
