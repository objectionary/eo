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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="to-java" version="2.0">
  <xsl:output encoding="UTF-8" method="text"/>
  <!-- Variables -->
  <xsl:variable name="delta">
    <select>Δ</select>
  </xsl:variable>
  <xsl:variable name="alpha">
    <select>α</select>
  </xsl:variable>
  <xsl:variable name="phi">
    <select>φ</select>
  </xsl:variable>
  <xsl:variable name="rho">
    <select>ρ</select>
  </xsl:variable>
  <xsl:variable name="home">
    <select>σ</select>
  </xsl:variable>
  <xsl:variable name="program">
    <select>Φ</select>
  </xsl:variable>
  <xsl:variable name="lambda">
    <select>λ</select>
  </xsl:variable>
  <xsl:variable name="vertex">
    <select>ν</select>
  </xsl:variable>
  <xsl:variable name="arrow">
    <select> ↦ </select>
  </xsl:variable>
  <!-- Functions -->
  <xsl:function name="eo:specials">
    <xsl:param name="n"/>
    <xsl:choose>
      <xsl:when test="$n='@'">
        <xsl:value-of select="$phi"/>
      </xsl:when>
      <xsl:when test="$n='.@'">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$phi"/>
      </xsl:when>
      <xsl:when test="$n='^'">
        <xsl:value-of select="$rho"/>
      </xsl:when>
      <xsl:when test="$n='.^'">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$rho"/>
      </xsl:when>
      <xsl:when test="$n='&amp;'">
        <xsl:value-of select="$home"/>
      </xsl:when>
      <xsl:when test="$n='.&amp;'">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$home"/>
      </xsl:when>
      <xsl:when test="$n='$'">
        <xsl:value-of select="$rho"/>
      </xsl:when>
      <xsl:when test="$n='&lt;'">
        <xsl:value-of select="$vertex"/>
      </xsl:when>
      <xsl:when test="$n='.&lt;'">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$vertex"/>
      </xsl:when>
      <xsl:when test="$n='Q'">
        <xsl:value-of select="$program"/>
      </xsl:when>
      <xsl:when test="starts-with($n, 'org.eolang')">
        <xsl:value-of select="$program"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$n"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$n"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="eo:bytes">
    <xsl:param name="bts"/>
    <xsl:choose>
      <xsl:when test="string-length($bts)>2">
        <xsl:for-each select="tokenize($bts, ' ')">
          <xsl:if test="position()>1">
            <xsl:text>-</xsl:text>
          </xsl:if>
          <xsl:value-of select="."/>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="string-length($bts)=2">
        <xsl:value-of select="$bts"/>
        <xsl:text>-</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>--</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="eo:comma">
    <xsl:param name="pos"/>
    <xsl:if test="$pos>1">
      <xsl:text>, </xsl:text>
    </xsl:if>
  </xsl:function>
  <!-- Program -->
  <xsl:template match="program">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates select="objects"/>
    <xsl:text>]</xsl:text>
  </xsl:template>
  <!-- Objects  -->
  <xsl:template match="objects">
    <xsl:for-each select="o">
      <xsl:value-of select="eo:comma(position())"/>
      <xsl:apply-templates select="."/>
    </xsl:for-each>
  </xsl:template>
  <!-- Free attribute -->
  <xsl:template match="o[parent::o[not(@base)] and not(@base) and not(@atom) and not(o)]">
    <xsl:value-of select="./@name"/>
    <xsl:value-of select="$arrow"/>
    <xsl:text>?</xsl:text>
  </xsl:template>
  <!-- Just object -->
  <xsl:template match="o[@base]">
    <xsl:if test="@name">
      <xsl:value-of select="eo:specials(@name)"/>
      <xsl:value-of select="$arrow"/>
    </xsl:if>
    <xsl:choose>
      <!-- Not method -->
      <xsl:when test="not(starts-with(@base, '.'))">
        <xsl:value-of select="eo:specials(@base)"/>
        <xsl:if test="count(o)>0">
          <xsl:text>(</xsl:text>
          <xsl:for-each select="o">
            <xsl:apply-templates select="." mode="application">
              <xsl:with-param name="position" select="position()"/>
            </xsl:apply-templates>
          </xsl:for-each>
          <xsl:text>)</xsl:text>
        </xsl:if>
      </xsl:when>
      <!-- Method -->
      <xsl:otherwise>
        <xsl:apply-templates select="o[position()=1]"/>
        <xsl:value-of select="eo:specials(@base)"/>
        <xsl:if test="count(o)>1">
          <xsl:text>(</xsl:text>
          <xsl:for-each select="o[position()!=1]">
            <xsl:apply-templates select="." mode="application">
              <xsl:with-param name="position" select="position()"/>
            </xsl:apply-templates>
          </xsl:for-each>
          <xsl:text>)</xsl:text>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
    <!-- Data -->
    <xsl:if test="@data">
      <xsl:text>(</xsl:text>
      <xsl:value-of select="$delta"/>
      <xsl:value-of select="$arrow"/>
      <xsl:value-of select="eo:bytes(.)"/>
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>
  <!-- Formation -->
  <xsl:template match="o[not(@base) and (@abstract or @atom)]">
    <xsl:if test="@name">
      <xsl:value-of select="eo:specials(@name)"/>
      <xsl:value-of select="$arrow"/>
    </xsl:if>
    <xsl:text>[</xsl:text>
    <xsl:if test="@atom">
      <xsl:value-of select="$lambda"/>
      <xsl:value-of select="$arrow"/>
      <xsl:text>lambda</xsl:text>
      <xsl:if test="count(o)>0">
        <xsl:text>, </xsl:text>
      </xsl:if>
    </xsl:if>
    <xsl:for-each select="o">
      <xsl:value-of select="eo:comma(position())"/>
      <xsl:apply-templates select="."/>
    </xsl:for-each>
    <xsl:text>]</xsl:text>
  </xsl:template>
  <!-- Application -->
  <xsl:template match="o" mode="application">
    <xsl:param name="position" select="1"/>
    <xsl:value-of select="eo:comma($position)"/>
    <xsl:choose>
      <xsl:when test="@as">
        <xsl:value-of select="eo:specials(@as)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$alpha"/>
        <xsl:value-of select="$position - 1"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="$arrow"/>
    <xsl:apply-templates select="."/>
  </xsl:template>
  <!-- Ignore other elements -->
  <xsl:template match="node()|@*">
    <xsl:apply-templates/>
  </xsl:template>
</xsl:stylesheet>
