<!-- Define the elements and attributes of the module -->
<!ELEMENT %Post.when.qname; EMPTY >
<!ATTLIST %Post.when.qname;
     year  CDATA #IMPLIED
     month CDATA #IMPLIED
     day   CDATA #IMPLIED
     %Post.Common.attrib;>

<!ELEMENT %Post.tag.qname; (#PCDATA) >
<!ATTLIST %Post.tag.qname;
     %Post.Common.attrib; >

<!-- Model for our module - how our dtd patches into xhtml -->

<!ENTITY % HeadOpts.mix
     "( %script.qname; | %style.qname; | %meta.qname;
      | %link.qname; | %object.qname; | %Post.tag.qname; | %Post.when.qname; )*"
>
