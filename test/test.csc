function enumNodes(node, level = 0)
{
  for(var i = 0; i < level; ++i)
  {
    print("  ");
  }

  if(node.nodeTypeName == "#element")
  {
    print(node.qualifiedName + " (" + node.nodeTypeName + ")");
  }
  else
  {
    print(node.nodeName + " (" + node.nodeTypeName + ")");
  }
  
  if(node.nodeTypeName == "#document" || 
     node.nodeTypeName == "#element"  ||
     node.nodeTypeName == "#processing-instruction" )
  {
    for(var att in node.attributes)
    {
	    print(" " + att.qualifiedName + "=\"" + att.value + "\"");	
    }
  }

  print("\n");

  if(node.nodeTypeName == "#document" || 
     node.nodeTypeName == "#element"  )
  {
    for(var elm in node.childNodes)
    {
	    enumNodes(elm, level + 1);
    }
  }
}

enumNodes(parseXml("d:\\source\\cscript\\test\\sample.xml"));