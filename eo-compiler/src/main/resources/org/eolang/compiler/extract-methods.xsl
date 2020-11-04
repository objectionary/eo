<?xml version="1.0"?>
<!--
The MIT License (MIT)

Copyright (c) 2017-2019 Yegor Bugayenko

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xsl:function name="eo:abstract" as="xs:boolean">
    <xsl:param name="object" as="element()"/>
    <xsl:sequence select="not(exists($object/@base)) and exists($object/o)"/>
  </xsl:function>
  <xsl:template match="/">
    <xsl:apply-templates select="node()|@*"/>
  </xsl:template>
  <xsl:template match="/program/o[eo:abstract(.) and exists(./o[eo:abstract(.)])]">
    <xsl:copy>
      <xsl:apply-templates select="o[not(eo:abstract(.))]|@*"/>
      <xsl:for-each select="o[eo:abstract(.)]">
        <xsl:element name="o">
          <xsl:attribute name="name">
            <xsl:value-of select="@name"/>
          </xsl:attribute>
          <xsl:attribute name="base">
            <xsl:value-of select="../@name"/>
            <xsl:text>$</xsl:text>
            <xsl:value-of select="@name"/>
          </xsl:attribute>
        </xsl:element>
      </xsl:for-each>
    </xsl:copy>
    <xsl:for-each select="o[eo:abstract(.)]">
      <xsl:copy>
        <xsl:attribute name="name">
          <xsl:value-of select="../@name"/>
          <xsl:text>$</xsl:text>
          <xsl:value-of select="@name"/>
        </xsl:attribute>
        <xsl:for-each select="../o[@name and not(o) and not(@base)]">
          <xsl:element name=""/>
        </xsl:for-each>
        <xsl:apply-templates select="node()|@* except @name"/>
      </xsl:copy>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
