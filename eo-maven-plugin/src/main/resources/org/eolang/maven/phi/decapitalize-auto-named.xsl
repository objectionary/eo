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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="decapitalize-auto-named" version="2.0">
  <!--
  Such EO code:
  x
    [] >>

  is translated to such XMIR:
  <o base="x">
    <o abstract="" and name="OBJ-2-2"/>
  </o>

  Object with name "OBJ-2-2" can't be converted to PHI because attribute should start with
  lowercase letter. So this transformation converts this name to "obj-2-2"
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="//o[@abstract and starts-with(@name,'OBJ-')]">
    <xsl:copy>
      <xsl:attribute name="name">
        <xsl:value-of select="string-join(('obj', substring-after(@name,'-')),'-')"/>
      </xsl:attribute>
      <xsl:apply-templates select="node()|(@* except @name)"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="//o[not(@name) and starts-with(@base,'OBJ-')]">
    <xsl:copy>
      <xsl:attribute name="base">
        <xsl:value-of select="string-join(('obj', substring-after(@base,'-')),'-')"/>
      </xsl:attribute>
      <xsl:apply-templates select="node()|(@* except @base)"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
