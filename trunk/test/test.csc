var xmlUnknown = 0;
var xmlElement = 1;
var xmlAttribute = 2; 	
var xmlText = 3;
var xmlCDATASection = 4;
var xmlEntityReference = 5; 	
var xmlEntity = 6;
var xmlProcessingInstruction = 7;
var xmlComment = 8;
var xmlDocument = 9;
var xmlDocumentType = 10;
var xmlDocumentFragment = 11;
var xmlNotation = 12;

var indent = "  ";

function enumNodes(node)
{
  if(node.nodeType == xmlDocument)
  {
    for(var elm in node.childNodes)
    {
      enumNodes(elm);
    }
    return;
  }

  if(node.nodeType == xmlElement)
  {
    print("<" + node.qualifiedName);
    
    for(var att in node.attributes)
    {
	    print(" " + att.qualifiedName + "=\"" + att.value + "\"");
    }
    
    if(count(node.childNodes))
    {
      print(">");
      for(var elm in node.childNodes)
      {
	      enumNodes(elm);
      }
      print("</" + node.qualifiedName + ">");
    }
    else
    {
      print("/>");
    }

    return;
  }

  if(node.nodeType == xmlText)
  {
    print(node.data);
    return;
  }
}

var xml = parseXml("d:\\source\\cscript\\test\\sample.xml");
print("Tree contains " + xml.nodeCount + " nodes\n");
enumNodes(xml);
print(collect());
