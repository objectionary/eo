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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="cti-adds-errors" version="2.0">
  <!--
  For every cti objects add error messages.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/errors">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:for-each select="//o[@base='cti']">
        <xsl:element name="error">
          <xsl:attribute name="check">
            <xsl:text>cti</xsl:text>
          </xsl:attribute>
          <xsl:attribute name="line">
            <xsl:value-of select="@line"/>
          </xsl:attribute>
          <xsl:attribute name="severity">
            <xsl:value-of select="eo:hex-to-utf8(element()[last() - 1])"/>
          </xsl:attribute>
          <xsl:value-of select="eo:hex-to-utf8(element()[last()])"/>
        </xsl:element>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <!--Converts hex sting into readable UTF-8 string-->
  <xsl:function name="eo:hex-to-utf8">
    <xsl:param name="str"/>
    <xsl:variable name="hex">0123456789ABCDEF</xsl:variable>
    <xsl:variable name="tail" select="translate($str, ' ', '')"/>
    <xsl:if test="$tail">
      <!-- extract first 2 digits -->
      <xsl:variable name="first" select="substring($tail, 1, 1)"/>
      <xsl:variable name="second" select="substring($tail, 2, 1)"/>
      <!-- get their hex values -->
      <xsl:variable name="val1" select="string-length(substring-before($hex, $first))"/>
      <xsl:variable name="val2" select="string-length(substring-before($hex, $second))"/>
      <!-- get the corresponding utf-8 character -->
      <xsl:variable name="head" select="codepoints-to-string($val1 * 16 + $val2)"/>
      <!-- recursive call with the rest of the hex string -->
      <xsl:value-of select="concat($head, eo:hex-to-utf8(substring($tail, 3)))"/>
    </xsl:if>
  </xsl:function>
</xsl:stylesheet>
