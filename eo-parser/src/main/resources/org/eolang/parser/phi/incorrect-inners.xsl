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
<xsl:stylesheet xmlns:eo="https://www.eolang.org" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="incorrect-inners" version="2.0">
  <!--
    Here we catch an elements which does not have @base attribute or which are not abstract
    but have inner elements.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/errors">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:for-each select="//o[count(o)&gt;0 and not(eo:abstract(.)) and not(@base)]">
        <xsl:element name="error">
          <xsl:attribute name="check">
            <xsl:text>incorrect-inners</xsl:text>
          </xsl:attribute>
          <xsl:attribute name="line">
            <xsl:choose>
              <xsl:when test="@line">
                <xsl:value-of select="@line"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:text>No line</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
          <xsl:attribute name="severity">
            <xsl:text>critical</xsl:text>
          </xsl:attribute>
          <xsl:text>Element can't have child elements if it does not have 'base' attribute or if it isn't abstract</xsl:text>
          <xsl:if test="@name">
            <xsl:text>. Name of the object - </xsl:text>
            <xsl:value-of select="@name"/>
          </xsl:if>
        </xsl:element>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
