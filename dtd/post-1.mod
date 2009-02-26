

<!-- Define the global namespace attributes -->
<![%Post.prefixed;[
<!ENTITY % Post.xmlns.attrib
    "%NS.decl.attrib;"
>
]]>
<!ENTITY % Post.xmlns.attrib
     "%NS.decl.attrib;
     xmlns  %URI.datatype;  #FIXED '%Post.xmlns;'"
>

<!-- Define a common set of attributes for all module elements -->
<!ENTITY % Post.Common.attrib
  "%Post.xmlns.attrib;
  id ID #IMPLIED"
>

<!-- Define the elements and attributes of the module -->
<!ELEMENT %Post.tag.qname; >
<!ATTLIST %Post.tag.qname;
     %Post.Common.attrib; >

<!ELEMENT %Post.when.qname; >
<!ATTLIST %Post.when.qname;
     year  CDATA #IMPLIED
     month CDATA #IMPLIED
     day   CDATA #IMPLIED
     %Post.Common.attrib;>

