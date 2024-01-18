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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="unroll-refs" version="2.0">
  <!--
  Here we find all objects refer to other objects through @ref
  attributes. Then, we replace them with composite XML structures
  that denote a chain of dot notation calls.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@base=//o[@level]/@name and @ref=//o[@level]/@line]">
    <xsl:variable name="o" select="."/>
    <xsl:copy>
      <xsl:attribute name="base">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="./@base"/>
      </xsl:attribute>
      <xsl:apply-templates select="@* except @base"/>
      <xsl:variable name="ref" select="//o[@line = $o/@ref and @name=$o/@base]"/>
      <xsl:if test="count($ref) != 1">
        <xsl:message terminate="yes">
          <xsl:text>Exactly one object named '</xsl:text>
          <xsl:value-of select="$o/@base"/>
          <xsl:text>' with @line equal to '</xsl:text>
          <xsl:value-of select="$o/@ref"/>
          <xsl:text>' is expected in the document, but </xsl:text>
          <xsl:value-of select="count($ref)"/>
          <xsl:text> of them found</xsl:text>
        </xsl:message>
      </xsl:if>
      <xsl:call-template name="up">
        <xsl:with-param name="level" select="$ref/@level"/>
        <xsl:with-param name="loc" select="$o/@loc"/>
      </xsl:call-template>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template name="up">
    <xsl:param name="level"/>
    <xsl:param name="loc"/>
    <xsl:element name="o">
      <xsl:attribute name="loc">
        <xsl:value-of select="$loc"/>
        <xsl:text>.ρ</xsl:text>
      </xsl:attribute>
      <xsl:attribute name="base">
        <xsl:choose>
          <xsl:when test="$level = 0">
            <xsl:text>$</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>.^</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:if test="$level &gt; 0">
        <xsl:call-template name="up">
          <xsl:with-param name="level" select="$level - 1"/>
          <xsl:with-param name="loc" select="concat($loc, '.ρ')"/>
        </xsl:call-template>
      </xsl:if>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
