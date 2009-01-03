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

var xmlDoc = cocreate("Microsoft.XMLDOM");
xmlDoc.async = false;
xmlDoc.preserveWhitespace = false;
xmlDoc.load("D:\\pronto\\6.0\\Include\\Instellingen.xml");

enumNodes(xmlDoc);

collect();
dump();