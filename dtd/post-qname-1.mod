<!-- Qualified names for our module -->

<!ENTITY % NS.prefixed "post" >
<!ENTITY % Post.prefixed "%NS.prefixed;" >

<!-- Declare the actual namespace of this module -->
<!ENTITY % Post.xmlns "http://hugoduncan.org/xmlns/post" >

<!-- Declare the default prefix for this module -->
<!ENTITY % Post.prefix "post" >

<!-- If this module's namespace is prefixed -->
<![%Post.prefixed;[
  <!ENTITY % Post.pfx  "%Post.prefix;:" >
]]>
<!ENTITY % Post.pfx  "" >

<!-- This entity is ALWAYS prefixed, for use when adding our
     attributes to an element in another namespace -->
<!ENTITY % Post.xmlns.attrib.prefixed
   "xmlns:%Post.prefix;  %URI.datatype;  #FIXED '%Post.xmlns;'"
>

<!-- Declare a Parameter Entity (PE) that defines any external namespaces
     that are used by this module -->
<!ENTITY % Post.xmlns.extra.attrib "" >

<!-- If we want to use xhtml namespace attributes on our elements, then
     we need a prefix for them; default to xhtml. -->
<!ENTITY % XHTML.prefix "xhtml" >

<!-- Declare a PE that defines the xmlns attributes for use by Post. -->
<![%Post.prefixed;[
<!ENTITY % Post.xmlns.attrib
   "%Post.xmlns.attrib.prefixed;
    %Post.xmlns.extra.attrib;"
>
<!-- Make sure that the Post namespace attributes are included on the XHTML
     attribute set -->
<!ENTITY % XHTML.xmlns.extra.attrib
    "xmlns:%XHTML.prefix;  %URI.datatype;  #FIXED 'http://www.w3.org/1999/xhtml'
    %Post.xmlns.attrib;" >
]]>
<!-- if we are not prefixed, then our elements should have the default
     namespace AND the prefixed namespace is added to the XHTML set
     because our attributes can be referenced on those elements
-->
<!ENTITY % Post.xmlns.attrib
   "xmlns   %URI.datatype;  #FIXED '%Post.xmlns;'
    %Post.xmlns.extra.attrib;"
>
<!ENTITY % XHTML.xmlns.extra.attrib
   "xmlns:%XHTML.prefix;  %URI.datatype;  #FIXED 'http://www.w3.org/1999/xhtml'
    %Post.xmlns.attrib.prefixed;"
>

<!-- Now declare the qualified names for all of the elements in the
     module -->
<!ENTITY % Post.when.qname "%Post.pfx;when" >
<!ENTITY % Post.updated.qname "%Post.pfx;updated" >
<!ENTITY % Post.linkname.qname "%Post.pfx;linkname" >
<!ENTITY % Post.tag.qname "%Post.pfx;tag" >


