<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2022 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="xmir-to-gmi" version="2.0">
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:function name="eo:index" as="xs:string">
    <xsl:param name="o" as="node()"/>
    <xsl:variable name="ret">
      <xsl:choose>
        <xsl:when test="name($o) = 'o'">
          <xsl:value-of select="count($o/ancestor::o) + count($o/preceding::o) + 1"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>0</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:function name="eo:vertex" as="xs:string">
    <xsl:param name="o" as="node()"/>
    <xsl:variable name="ret">
      <xsl:text>v</xsl:text>
      <xsl:value-of select="eo:index($o)"/>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:function name="eo:edge" as="xs:string">
    <xsl:param name="o1" as="node()"/>
    <xsl:param name="o2" as="node()"/>
    <xsl:variable name="ret">
      <xsl:text>e</xsl:text>
      <xsl:value-of select="eo:index($o1)"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of select="eo:index($o2)"/>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:template name="i">
    <xsl:param name="name" as="xs:string"/>
    <xsl:param name="args" as="item()*"/>
    <xsl:param name="comment" as="xs:string"/>
    <xsl:element name="i">
      <xsl:attribute name="name">
        <xsl:value-of select="$name"/>
      </xsl:attribute>
      <xsl:for-each select="$args">
        <xsl:element name="a">
          <xsl:value-of select="."/>
        </xsl:element>
      </xsl:for-each>
      <xsl:element name="c">
        <xsl:value-of select="$comment"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>
</xsl:stylesheet>
