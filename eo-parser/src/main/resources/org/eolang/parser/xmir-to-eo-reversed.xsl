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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="xmir-to-eo-reversed" version="2.0">
  <!--
  This one maps XMIR to EO original syntax in reversed notation.
  It's used in XmirReversed.java class.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:variable name="eol" select="'&#10;'"/>
  <xsl:variable name="comment">
    <xsl:text># This is the default 64+ symbols comment in front of named abstract object.</xsl:text>
    <xsl:value-of select="$eol"/>
  </xsl:variable>
  <xsl:output method="text" encoding="UTF-8"/>
  <!-- PROGRAM -->
  <xsl:template match="program">
    <eo>
      <xsl:apply-templates select="license"/>
      <xsl:apply-templates select="metas[meta]"/>
      <xsl:apply-templates select="objects"/>
    </eo>
  </xsl:template>
  <!-- LICENCE -->
  <xsl:template match="license">
    <xsl:for-each select="tokenize(., $eol)">
      <xsl:text># </xsl:text>
      <xsl:value-of select="."/>
      <xsl:value-of select="$eol"/>
    </xsl:for-each>
    <xsl:if test="text()">
      <xsl:value-of select="$eol"/>
    </xsl:if>
  </xsl:template>
  <!-- METAS -->
  <xsl:template match="metas">
    <xsl:apply-templates select="meta"/>
    <xsl:value-of select="$eol"/>
  </xsl:template>
  <!-- META -->
  <xsl:template match="meta">
    <xsl:text>+</xsl:text>
    <xsl:value-of select="head"/>
    <xsl:if test="not(empty(tail/text()))">
      <xsl:text> </xsl:text>
      <xsl:value-of select="tail"/>
    </xsl:if>
    <xsl:value-of select="$eol"/>
  </xsl:template>
  <!-- OBJECTS -->
  <xsl:template match="objects">
    <xsl:apply-templates select="o"/>
  </xsl:template>
  <!-- OBJECT, NOT FREE ATTRIBUTE -->
  <xsl:template match="o[not(eo:attr(.))]">
    <xsl:param name="indent" select="''"/>
    <!--IF NOT THE FIRST TOP OBJECT -->
    <xsl:if test="position()&gt;1 and parent::objects">
      <xsl:value-of select="$eol"/>
    </xsl:if>
    <xsl:value-of select="$indent"/>
    <xsl:apply-templates select="." mode="head">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="." mode="tail"/>
    <xsl:value-of select="$eol"/>
    <xsl:apply-templates select="o[not(eo:attr(.))]">
      <xsl:with-param name="indent" select="concat('  ', $indent)"/>
    </xsl:apply-templates>
  </xsl:template>
  <!-- BASED -->
  <xsl:template match="o[not(@data) and @base]" mode="head">
    <xsl:choose>
      <xsl:when test="starts-with(@base,'.')">
        <xsl:value-of select="substring(@base,2)"/>
        <xsl:text>.</xsl:text>
      </xsl:when>
      <!-- NOT OPTIMIZED TUPLE -->
      <xsl:when test="@star">
        <xsl:text>*</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@base"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- ABSTRACT OR ATOM -->
  <xsl:template match="o[not(@data) and not(@base)]" mode="head">
    <xsl:param name="indent"/>
    <xsl:value-of select="$comment"/>
    <xsl:value-of select="$indent"/>
    <xsl:text>[</xsl:text>
    <xsl:for-each select="o[eo:attr(.)]">
      <xsl:if test="position()&gt;1">
        <xsl:text> </xsl:text>
      </xsl:if>
      <xsl:value-of select="@name"/>
    </xsl:for-each>
    <xsl:text>]</xsl:text>
  </xsl:template>
  <!-- TAIL: SUFFIX, NAME, CONST, COPY, ATOM -->
  <xsl:template match="o" mode="tail">
    <xsl:if test="@copy">
      <xsl:text>'</xsl:text>
    </xsl:if>
    <xsl:if test="@as">
      <xsl:text>:</xsl:text>
      <xsl:value-of select="@as"/>
    </xsl:if>
    <xsl:if test="@name">
      <xsl:text> &gt; </xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:if test="@const">
        <xsl:text>!</xsl:text>
      </xsl:if>
      <xsl:if test="@atom">
        <xsl:text> /</xsl:text>
        <xsl:value-of select="@atom"/>
      </xsl:if>
    </xsl:if>
  </xsl:template>
  <!-- DATA -->
  <xsl:template match="o[@data]" mode="head">
    <xsl:choose>
      <xsl:when test="@data='string'">
        <xsl:text>"</xsl:text>
        <xsl:value-of select="text()"/>
        <xsl:text>"</xsl:text>
      </xsl:when>
      <xsl:when test="@data='bool' or @data='int' or @data='float'">
        <xsl:value-of select="text()"/>
      </xsl:when>
      <xsl:when test="@data='bytes'">
        <xsl:choose>
          <xsl:when test="empty(text())">
            <xsl:text>--</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="replace(text(), ' ', '-')"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">
          <xsl:text>Invalid data attribute: </xsl:text>
          <xsl:value-of select="@data"/>
        </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
