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
<xsl:stylesheet xmlns:eo="https://www.eolang.org" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="wrap-default-package" version="2.0">
  <!--
    Here we change elements:

    <o base=".eolang">
      <o base=".org">
        <o base="Q"/>
      </o>
    </o>

    with:

    <o base="org.eolang"/>
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[starts-with(@base, '.') and o[1][@base='.eolang' and o[1][@base='.org' and o[1][@base='Q']]]]">
    <xsl:element name="o">
      <xsl:for-each select="@*[name()!='base']">
        <xsl:attribute name="{name()}" select="."/>
      </xsl:for-each>
      <xsl:attribute name="base">
        <xsl:text>org.eolang</xsl:text>
        <xsl:value-of select="@base"/>
      </xsl:attribute>
      <xsl:for-each select="o[position()!=1]">
        <xsl:apply-templates select="."/>
      </xsl:for-each>
      <xsl:if test="eo:has-data(.)">
        <xsl:value-of select="normalize-space(string-join(text(), ''))"/>
      </xsl:if>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
