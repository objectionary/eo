<?xml version="1.0"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2021 Yegor Bugayenko

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0">
  <!--
  This one maps XMIR to EO original syntax. It's used
  in XMIR.java class.
  -->
  <xsl:variable name="eol" select="'&#10;'"/>
  <xsl:output method="text"/>
  <xsl:strip-space elements="*"/>
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
  <xsl:template match="o[parent::o[not(@base)] and not(@base) and not(@atom)]">
    <!-- nothing, it's an free attribute -->
  </xsl:template>
  <xsl:template match="o">
    <xsl:param name="indent" select="''"/>
    <xsl:if test="position() &gt; 1 and parent::objects">
      <xsl:value-of select="$eol"/>
    </xsl:if>
    <xsl:value-of select="$indent"/>
    <xsl:apply-templates select="." mode="inside"/>
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
    <xsl:value-of select="$eol"/>
    <xsl:apply-templates select="o">
      <xsl:with-param name="indent" select="concat('  ', $indent)"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="o[not(@data)]" mode="inside">
    <xsl:choose>
      <xsl:when test="parent::o[not(@base)] and not(@base) and not(@atom)">
        <!-- nothing -->
      </xsl:when>
      <xsl:when test="@base">
        <xsl:choose>
          <xsl:when test="starts-with(@base, '.')">
            <xsl:value-of select="substring-after(@base, '.')"/>
            <xsl:text>.</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="@base"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>[</xsl:text>
        <xsl:for-each select="o[not(@base) and not(@atom)]">
          <xsl:if test="position() &gt; 1">
            <xsl:text> </xsl:text>
          </xsl:if>
          <xsl:value-of select="@name"/>
          <xsl:if test="@vararg">
            <xsl:text>...</xsl:text>
          </xsl:if>
        </xsl:for-each>
        <xsl:text>]</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="o[@data='string']" mode="inside">
    <xsl:text>"</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>"</xsl:text>
  </xsl:template>
  <xsl:template match="o[@data and @data!='string']" mode="inside">
    <xsl:value-of select="text()"/>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
