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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="vars-float-up" version="2.0">
  <!--
  If we see this code, where a name is defined inside
  an abstract objects:

  [] > test
    hello
      foo > x
        15

  We move "x" declaration to the nearest abstract object
  and make it its attribute:

  [] > test
    foo > x
      15
    hello
      x
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="objects">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()" mode="full"/>
      <xsl:for-each select="o/descendant::o[@name]">
        <xsl:if test="not(ancestor::o[eo:abstract(.)])">
          <xsl:apply-templates select="." mode="full"/>
        </xsl:if>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[eo:abstract(.) and not(../objects)]" priority="0">
    <xsl:apply-templates select="." mode="full"/>
  </xsl:template>
  <xsl:template match="o[eo:abstract(.)]" mode="full" priority="1">
    <xsl:variable name="o" select="."/>
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
      <xsl:for-each select="o/descendant::o[@name]">
        <xsl:if test="ancestor::o[eo:abstract(.)][1]/generate-id() = generate-id($o)">
          <xsl:apply-templates select="." mode="full"/>
        </xsl:if>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[@name and @name!='@' and ancestor::o[1][not(eo:abstract(.))]]">
    <xsl:element name="o">
      <xsl:attribute name="base">
        <xsl:value-of select="@name"/>
      </xsl:attribute>
      <xsl:attribute name="ref">
        <xsl:value-of select="@line"/>
      </xsl:attribute>
      <xsl:attribute name="cut">
        <xsl:value-of select="count(descendant::o)"/>
      </xsl:attribute>
      <xsl:apply-templates select="@line"/>
      <xsl:apply-templates select="@method"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
