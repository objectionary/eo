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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="to-text" version="2.0">
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="EOL">
    <xsl:value-of select="'&#10;'"/>
  </xsl:variable>
  <xsl:template match="/sodg">
    <xsl:element name="text">
      <xsl:apply-templates select="i"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="i[@name='COMMENT']">
    <xsl:value-of select="$EOL"/>
    <xsl:text># </xsl:text>
    <xsl:value-of select="c"/>
    <xsl:value-of select="$EOL"/>
  </xsl:template>
  <xsl:template match="i[@name!='COMMENT']">
    <xsl:value-of select="@name"/>
    <xsl:text>(</xsl:text>
    <xsl:for-each select="a">
      <xsl:if test="position() &gt; 1">
        <xsl:text>, </xsl:text>
      </xsl:if>
      <xsl:value-of select="."/>
    </xsl:for-each>
    <xsl:text>);</xsl:text>
    <xsl:if test="c and not(empty(c/text()))">
      <xsl:text> # </xsl:text>
      <xsl:value-of select="c"/>
    </xsl:if>
    <xsl:value-of select="$EOL"/>
  </xsl:template>
</xsl:stylesheet>
