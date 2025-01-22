<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2025 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="tests" version="2.0">
  <!--
  This stylesheet will take outer objects and put them into
  classes that are unit tests.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:function name="eo:test-name" as="xs:string">
    <xsl:param name="name" as="xs:string"/>
    <xsl:variable name="parts" select="tokenize($name, '\$')"/>
    <xsl:variable name="p">
      <xsl:for-each select="$parts">
        <xsl:if test="position()&gt;1">
          <xsl:text>$</xsl:text>
        </xsl:if>
        <xsl:value-of select="."/>
        <xsl:if test="position()=1">
          <xsl:text>Test</xsl:text>
        </xsl:if>
      </xsl:for-each>
    </xsl:variable>
    <xsl:value-of select="$p"/>
  </xsl:function>
  <xsl:template match="class/@name">
    <xsl:attribute name="name">
      <xsl:choose>
        <xsl:when test="/program/metas/meta[head='tests']">
          <xsl:value-of select="eo:test-name(.)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
