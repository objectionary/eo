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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="expand-aliases" version="2.0">
  <!--
  Here we find all aliases that don't use full syntax
  and expand them to the full one. For example, this one:

  +alias org.example.foo

  Will be expanded to:

  +alias foo org.example.foo
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/metas/meta[head='alias']">
    <xsl:variable name="expanded" select="contains(tail, ' ')"/>
    <xsl:variable name="last">
      <xsl:choose>
        <xsl:when test="$expanded">
          <xsl:value-of select="tokenize(tail/text(), ' ')[last()]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="tail/text()"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="parts" select="tokenize($last, '\.')"/>
    <xsl:variable name="tail">
      <xsl:if test="not(starts-with($last, 'Q.'))">
        <xsl:text>Q.</xsl:text>
      </xsl:if>
      <xsl:value-of select="$last"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="node() except tail except part|@*"/>
      <xsl:element name="tail">
        <xsl:value-of select="$parts[last()]"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="$tail"/>
      </xsl:element>
      <xsl:element name="part">
        <xsl:value-of select="$parts[last()]"/>
      </xsl:element>
      <xsl:element name="part">
        <xsl:value-of select="$tail"/>
      </xsl:element>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
