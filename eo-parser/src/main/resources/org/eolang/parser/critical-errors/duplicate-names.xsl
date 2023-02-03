<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2023 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="duplicate-names" version="2.0">
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/errors">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:for-each select="//o|/program/objects">
        <xsl:apply-templates select="." mode="dups"/>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o|objects" mode="dups">
    <xsl:for-each select="o[@name]">
      <xsl:variable name="x" select="."/>
      <xsl:if test="preceding-sibling::o/@name = $x/@name">
        <xsl:element name="error">
          <xsl:attribute name="check">
            <xsl:text>duplicate-names</xsl:text>
          </xsl:attribute>
          <xsl:attribute name="line">
            <xsl:value-of select="@line"/>
          </xsl:attribute>
          <xsl:attribute name="severity">
            <xsl:text>critical</xsl:text>
          </xsl:attribute>
          <xsl:text>The name "</xsl:text>
          <xsl:value-of select="@name"/>
          <xsl:text>" is already in use</xsl:text>
        </xsl:element>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
