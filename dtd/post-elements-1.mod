
<!-- Define the elements and attributes of the module -->
<!ELEMENT %Post.when.qname; EMPTY >
<!ATTLIST %Post.when.qname;
     year  CDATA #REQUIRED
     month CDATA #REQUIRED
     day   CDATA #REQUIRED
>

<!ELEMENT %Post.updated.qname; EMPTY >
<!ATTLIST %Post.updated.qname;
     year  CDATA #REQUIRED
     month CDATA #REQUIRED
     day   CDATA #REQUIRED
>

<!ELEMENT %Post.tag.qname; (#PCDATA) >
<!ATTLIST %Post.tag.qname;
  %Post.xmlns.attrib; id ID #IMPLIED>

<!ELEMENT %Post.linkname.qname; (#PCDATA) >
<!ATTLIST %Post.linkname.qname; >

