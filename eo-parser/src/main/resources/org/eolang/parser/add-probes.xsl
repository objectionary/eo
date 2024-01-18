<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2024 Objectionary.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="add-probes" version="2.0">
  <!--
  For every object that starts with '.' add probe meta
  with fully qualified name of the object.
  Example:
   For object:
   <o base=".abc" line="23" ... >
     <o base="Q" ... >
     </o>
   </o>
   Meta will be added:
   <meta line="23">
     <head>probe</head>
     <tail>Q.abc</tail>
     <part>Q.abc</part>
   </meta>
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:function name="eo:contains-any-of" as="xs:boolean">
    <xsl:param name="original" as="xs:string"/>
    <xsl:param name="chars" as="xs:string*"/>
    <xsl:sequence select="some $char in $chars satisfies contains($original, $char)"/>
  </xsl:function>
  <xsl:function name="eo:qualify" as="xs:string">
    <xsl:param name="e" as="element()"/>
    <xsl:variable name="fco" select="$e/o[1]"/>
    <xsl:choose>
      <xsl:when test="starts-with($e/@base, '.')">
        <xsl:value-of select="concat(eo:qualify($fco), $e/@base)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$e/@base"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:template match="/program/metas">
    <xsl:variable name="mts">
      <xsl:copy>
        <xsl:apply-templates select="node()|@*"/>
        <xsl:for-each select="//o[starts-with(@base, '.')]">
          <xsl:variable name="p" select="eo:qualify(.)"/>
          <xsl:variable name="c" select="string-length($p) - string-length(translate($p, '.', ''))"/>
          <xsl:if test="not(eo:contains-any-of($p, ('$', '^', '@', '&lt;'))) and not(starts-with($p, '.')) and $c &gt; 1">
            <xsl:element name="meta">
              <xsl:attribute name="line">
                <xsl:value-of select="@line"/>
              </xsl:attribute>
              <xsl:element name="head">
                <xsl:text>probe</xsl:text>
              </xsl:element>
              <xsl:element name="tail">
                <xsl:value-of select="string-join(($p, @ver),'|')"/>
              </xsl:element>
              <xsl:element name="part">
                <xsl:value-of select="$p"/>
              </xsl:element>
            </xsl:element>
          </xsl:if>
        </xsl:for-each>
      </xsl:copy>
    </xsl:variable>
    <xsl:apply-templates select="$mts"/>
  </xsl:template>
  <xsl:template match="meta[head/text() = 'probe' and tail/text() = following::meta/tail/text()]"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
