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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" version="2.0">
  <!--
  This one maps XMIR to EO original syntax. It's used
  in XMIR.java class.

  @todo #1099:30m Current bytes to string conversion
   supports only ASCII characters & text blocks.
   Make it possible to handle any unicode character and
   double-quoted strings.
  @todo #1110:30m Add conversion from 'bytes' representation
   back to 'int', 'double' & the rest of types. Then proceed
   to with the parent todo.
  @todo #1110:30m Add XST transformation to convert
   "$bytes.as-$type" to "$type". I.e
   "01-.as-bool" becomes "TRUE". Remove analogous conversions
   from this stylesheet, and only generate "$bytes.as-$type"
   in order to covert byte-array value back to literal.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:variable name="eol" select="'&#10;'"/>
  <xsl:output method="text" encoding="UTF-8"/>
  <xsl:template match="program">
    <xsl:apply-templates select="license"/>
    <xsl:apply-templates select="metas[meta]"/>
    <xsl:apply-templates select="objects"/>
  </xsl:template>
  <xsl:template match="license">
    <xsl:for-each select="tokenize(., $eol)">
      <xsl:text># </xsl:text>
      <xsl:value-of select="."/>
      <xsl:value-of select="$eol"/>
    </xsl:for-each>
    <xsl:value-of select="$eol"/>
  </xsl:template>
  <xsl:template match="metas">
    <xsl:apply-templates select="meta"/>
    <xsl:value-of select="$eol"/>
  </xsl:template>
  <xsl:template match="meta">
    <xsl:text>+</xsl:text>
    <xsl:value-of select="head"/>
    <xsl:if test="not(empty(tail/text()))">
      <xsl:text> </xsl:text>
      <xsl:value-of select="tail"/>
    </xsl:if>
    <xsl:value-of select="$eol"/>
  </xsl:template>
  <xsl:template match="objects">
    <xsl:apply-templates select="o"/>
  </xsl:template>
  <xsl:template match="o[eo:attr(.)]">
    <!-- nothing, it's an free attribute -->
  </xsl:template>
  <xsl:template match="o[not(eo:attr(.)) and starts-with(@base, '.')]">
    <xsl:param name="indent" select="''"/>
    <xsl:apply-templates select="o[position() = 1]">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
    <xsl:value-of select="$indent"/>
    <xsl:apply-templates select="." mode="head"/>
    <xsl:apply-templates select="." mode="tail"/>
    <xsl:value-of select="$eol"/>
    <xsl:apply-templates select="o[position() &gt; 1]">
      <xsl:with-param name="indent" select="concat('  ', $indent)"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="o[not(eo:attr(.)) and not(starts-with(@base, '.'))]">
    <xsl:param name="indent" select="''"/>
    <xsl:if test="position() &gt; 1 and parent::objects">
      <xsl:value-of select="$eol"/>
    </xsl:if>
    <xsl:value-of select="$indent"/>
    <xsl:apply-templates select="." mode="head"/>
    <xsl:apply-templates select="." mode="tail"/>
    <xsl:value-of select="$eol"/>
    <xsl:apply-templates select="o">
      <xsl:with-param name="indent" select="concat('  ', $indent)"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="o" mode="tail">
    <xsl:if test="@as">
      <xsl:text>:</xsl:text>
      <xsl:value-of select="@as"/>
    </xsl:if>
    <xsl:if test="@name">
      <xsl:text> &gt; </xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:if test="@vararg">
        <xsl:text>...</xsl:text>
      </xsl:if>
      <xsl:if test="@const">
        <xsl:text>!</xsl:text>
      </xsl:if>
      <xsl:if test="@atom">
        <xsl:text> /</xsl:text>
        <xsl:value-of select="@atom"/>
      </xsl:if>
    </xsl:if>
  </xsl:template>
  <xsl:template match="o[not(@data) and @base]" mode="head">
    <xsl:value-of select="@base"/>
  </xsl:template>
  <xsl:template match="o[not(@data) and not(@base)]" mode="head">
    <xsl:text>[</xsl:text>
    <xsl:for-each select="o[eo:attr(.)]">
      <xsl:if test="position() &gt; 1">
        <xsl:text> </xsl:text>
      </xsl:if>
      <xsl:value-of select="@name"/>
      <xsl:if test="@vararg">
        <xsl:text>...</xsl:text>
      </xsl:if>
    </xsl:for-each>
    <xsl:text>]</xsl:text>
  </xsl:template>
  <xsl:template match="o[@data='array']" mode="head">
    <xsl:text>*</xsl:text>
  </xsl:template>
  <xsl:template match="o[@data='string']" mode="head">
    <xsl:text>"</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>"</xsl:text>
  </xsl:template>
  <xsl:template match="o[@data='bool']" mode="head">
    <xsl:value-of select="upper-case(text())"/>
  </xsl:template>
  <xsl:template match="o[@data and @data!='string' and @data!='array' and @data!='bool' and @data!='bytes']" mode="head">
    <xsl:value-of select="text()"/>
  </xsl:template>
  <xsl:template match="o[@data='bytes']" mode="head">
    <xsl:choose>
      <xsl:when test="@base='string'">
        <xsl:text>"""</xsl:text>
        <xsl:value-of select="$eol"/>
        <xsl:for-each select="tokenize(text(), ' ')">
          <xsl:value-of select="concat('\u00', .)"/>
        </xsl:for-each>
        <xsl:value-of select="$eol"/>
        <xsl:text>"""</xsl:text>
      </xsl:when>
      <xsl:when test="@base='bool'">
        <xsl:choose>
          <xsl:when test="text() = '01'">
            <xsl:text>TRUE</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>FALSE</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="@base='int'">
        <xsl:value-of select="eo:bytes-to-int(replace(text(), ' ', ''))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="replace(text(), ' ', '-')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
