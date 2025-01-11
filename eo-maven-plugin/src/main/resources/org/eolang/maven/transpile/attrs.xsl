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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="attrs" version="2.0">
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="class[not(@base)]/o[@name]">
    <xsl:apply-templates select="." mode="with-attributes"/>
  </xsl:template>
  <xsl:template match="o/o[@name]" mode="abstracts">
    <xsl:apply-templates select="." mode="with-attributes"/>
  </xsl:template>
  <xsl:template match="*" mode="with-attributes">
    <xsl:element name="attr">
      <xsl:apply-templates select="@name"/>
      <xsl:variable name="type">
        <xsl:choose>
          <xsl:when test="@base and @base!='∅'">
            <xsl:text>bound</xsl:text>
          </xsl:when>
          <xsl:when test="@base and @base='∅'">
            <xsl:text>void</xsl:text>
          </xsl:when>
          <xsl:when test="@atom">
            <xsl:text>atom</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>abstract</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="inner">
        <xsl:element name="{$type}">
          <xsl:if test="$type='abstract'">
            <xsl:apply-templates select="@*"/>
            <xsl:apply-templates select="node()" mode="abstracts"/>
          </xsl:if>
          <xsl:if test="$type!='abstract'">
            <xsl:copy>
              <xsl:apply-templates select="node()|@*"/>
            </xsl:copy>
          </xsl:if>
        </xsl:element>
      </xsl:variable>
      <xsl:copy-of select="$inner"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
