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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="classes" version="2.0">
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template name="serialize">
    <xsl:param name="node"/>
    <xsl:param name="indent"/>
    <xsl:variable name="name" select="name($node)"/>
    <xsl:value-of select="$indent"/>
    <xsl:text>&lt;</xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:for-each select="$node/@*">
      <xsl:text> </xsl:text>
      <xsl:value-of select="name()"/>
      <xsl:text>="</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>"</xsl:text>
    </xsl:for-each>
    <xsl:variable name="content" select="replace(text()[1], '^\s*(.+?)\s*$', '$1')"/>
    <xsl:if test="$content = '' and not($node/element())">
      <xsl:text>/</xsl:text>
    </xsl:if>
    <xsl:text>&gt;</xsl:text>
    <xsl:value-of select="$content"/>
    <xsl:if test="$node/element()">
      <xsl:for-each select="$node/element()">
        <xsl:value-of select="'&#10;'"/>
        <xsl:call-template name="serialize">
          <xsl:with-param name="node" select="."/>
          <xsl:with-param name="indent" select="concat($indent, '  ')"/>
        </xsl:call-template>
      </xsl:for-each>
      <xsl:value-of select="'&#10;'"/>
      <xsl:value-of select="$indent"/>
    </xsl:if>
    <xsl:if test="$content != '' or $node/element()">
      <xsl:text>&lt;/</xsl:text>
      <xsl:value-of select="$name"/>
      <xsl:text>&gt;</xsl:text>
    </xsl:if>
  </xsl:template>
  <xsl:template match="o[eo:abstract(.)]">
    <xsl:element name="class">
      <xsl:apply-templates select="node()|@*"/>
      <xsl:element name="xmir">
        <xsl:call-template name="serialize">
          <xsl:with-param name="node" select="."/>
          <xsl:with-param name="indent" select="''"/>
        </xsl:call-template>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
