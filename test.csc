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
	    print(" " + att.qualifiedName + "=" + att.value);	
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


var x = 
<?xml version="1.0" ?>
<root xmlns="http://bla/root"
  xmlns:x="http://bla-x/root"
  xmlns:y="http://bla-y/root">
  <x:nested y:attr1="value1" y:attr2="value2">bla bla</x:nested>
  <y:empty x:attr1="value1" x:attr2="value2"/>
</root>;

enumNodes(x);