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

function enumNodes(node, level = 0)
{
  for(var i = 0; i < level; ++i)
  {
    print(indent);
  }

  if(node.nodeType == xmlElement)
  {
    print(node.qualifiedName + " (" + node.nodeTypeName + ")");
  }
  else if(node.nodeType == xmlText)
  {
    print(node.data);
  }
  else
  {
    print(node.nodeName + " (" + node.nodeTypeName + ")");
  }
  
  if(node.nodeType == xmlDocument || 
     node.nodeType == xmlElement  )
  {
    for(var att in node.attributes)
    {
	    print(" " + att.qualifiedName + "=\"" + att.value + "\"");	
    }
  }

  print("\n");

  if(node.nodeType == xmlDocument || 
     node.nodeType == xmlElement  )
  {
    for(var elm in node.childNodes)
    {
	    enumNodes(elm, level + 1);
    }
  }
}

var xml = parseXml("d:\\source\\cscript\\test\\sample.xml");
print("Tree contains " + xml.nodeCount + " nodes\n");
//enumNodes(xml);
print(collect());
