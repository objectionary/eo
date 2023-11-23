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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="to-phi" version="2.0">
  <xsl:output encoding="UTF-8" method="text"/>
  <!-- Variables -->
  <xsl:variable name="xi">
    <select>ξ</select>
  </xsl:variable>
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
  <xsl:variable name="dashed-arrow">
    <select> ⤍ </select>
  </xsl:variable>
  <xsl:variable name="lb">
    <select>⟦</select>
  </xsl:variable>
  <xsl:variable name="rb">
    <select>⟧</select>
  </xsl:variable>
  <xsl:variable name="empty">
    <select>∅</select>
  </xsl:variable>
  <!-- Functions -->
  <xsl:function name="eo:add-xi">
    <xsl:param name="add"/>
    <xsl:if test="$add">
      <xsl:value-of select="$xi"/>
      <xsl:text>.</xsl:text>
    </xsl:if>
  </xsl:function>
  <xsl:function name="eo:specials">
    <xsl:param name="n"/>
    <xsl:param name="is-name"/>
    <xsl:choose>
      <xsl:when test="$n='@'">
        <xsl:value-of select="eo:add-xi(not($is-name))"/>
        <xsl:value-of select="$phi"/>
      </xsl:when>
      <xsl:when test="$n='.@'">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$phi"/>
      </xsl:when>
      <xsl:when test="$n='^'">
        <xsl:value-of select="eo:add-xi(not($is-name))"/>
        <xsl:value-of select="$rho"/>
      </xsl:when>
      <xsl:when test="$n='.^'">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$rho"/>
      </xsl:when>
      <xsl:when test="$n='&amp;'">
        <xsl:value-of select="eo:add-xi(not($is-name))"/>
        <xsl:value-of select="$home"/>
      </xsl:when>
      <xsl:when test="$n='.&amp;'">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$home"/>
      </xsl:when>
      <xsl:when test="$n='$'">
        <xsl:value-of select="eo:add-xi(not($is-name))"/>
        <xsl:value-of select="$rho"/>
      </xsl:when>
      <xsl:when test="$n='&lt;'">
        <xsl:value-of select="eo:add-xi(not($is-name))"/>
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
        <xsl:value-of select="eo:add-xi(not($is-name))"/>
        <xsl:value-of select="$n"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="eo:bytes">
    <xsl:param name="bts"/>
    <xsl:choose>
      <xsl:when test="string-length($bts)&gt;2">
        <xsl:for-each select="tokenize($bts, ' ')">
          <xsl:if test="position()&gt;1">
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
    <xsl:if test="$pos&gt;1">
      <xsl:text>, </xsl:text>
    </xsl:if>
  </xsl:function>
  <!-- Program -->
  <xsl:template match="program">
    <xsl:text>{</xsl:text>
    <xsl:variable name="has-package" select="metas/meta/head[text()='package']"/>
    <xsl:variable name="parts" select="tokenize(metas/meta[head[text()='package']]/tail[1], '\.')"/>
    <xsl:choose>
      <xsl:when test="$has-package">
        <xsl:for-each select="$parts">
          <xsl:value-of select="."/>
          <xsl:value-of select="$arrow"/>
          <xsl:value-of select="$lb"/>
        </xsl:for-each>
        <xsl:apply-templates select="objects"/>
        <xsl:for-each select="$parts">
          <xsl:value-of select="$rb"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="objects"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>}</xsl:text>
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
    <xsl:value-of select="$empty"/>
  </xsl:template>
  <!-- Find path template -->
  <xsl:template match="o" mode="path">
    <xsl:param name="find"/>
    <xsl:variable name="parent" select="parent::o"/>
    <xsl:choose>
      <xsl:when test="$parent[not(@abstract)]">
        <xsl:apply-templates select="$parent" mode="path">
          <xsl:with-param name="find" select="$find"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:when test="$parent[@abstract]">
        <xsl:choose>
          <xsl:when test="@base=$find">
            <xsl:value-of select="eo:specials('^', true())"/>
            <xsl:text>.</xsl:text>
          </xsl:when>
          <xsl:when test="not(o[@base=$find])">
            <xsl:value-of select="eo:specials('^', true())"/>
            <xsl:text>.</xsl:text>
            <xsl:apply-templates select="$parent" mode="path">
              <xsl:with-param name="find" select="$find"/>
            </xsl:apply-templates>
          </xsl:when>
        </xsl:choose>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <!-- Just object -->
  <xsl:template match="o[@base]">
    <xsl:if test="@name">
      <xsl:value-of select="eo:specials(@name, true())"/>
      <xsl:value-of select="$arrow"/>
    </xsl:if>
    <xsl:choose>
      <!-- Not method -->
      <xsl:when test="not(starts-with(@base, '.'))">
        <xsl:choose>
          <xsl:when test="@ref">
            <xsl:value-of select="eo:add-xi(true())"/>
            <xsl:apply-templates select="." mode="path">
              <xsl:with-param name="find" select="@base"/>
            </xsl:apply-templates>
            <xsl:value-of select="eo:specials(@base, true())"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="eo:specials(@base, false())"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:if test="count(o)&gt;0">
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
        <xsl:value-of select="eo:specials(@base, true())"/>
        <xsl:if test="count(o)&gt;1">
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
      <xsl:value-of select="$dashed-arrow"/>
      <xsl:value-of select="eo:bytes(.)"/>
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>
  <!-- Formation -->
  <xsl:template match="o[not(@base) and (@abstract or @atom)]">
    <xsl:if test="@name">
      <xsl:value-of select="eo:specials(@name, true())"/>
      <xsl:value-of select="$arrow"/>
    </xsl:if>
    <xsl:value-of select="$lb"/>
    <xsl:if test="@atom">
      <xsl:value-of select="$lambda"/>
      <xsl:value-of select="$dashed-arrow"/>
      <xsl:text>Lambda</xsl:text>
      <xsl:if test="count(o)&gt;0">
        <xsl:text>, </xsl:text>
      </xsl:if>
    </xsl:if>
    <xsl:for-each select="o">
      <xsl:value-of select="eo:comma(position())"/>
      <xsl:apply-templates select="."/>
    </xsl:for-each>
    <xsl:value-of select="$rb"/>
  </xsl:template>
  <!-- Application -->
  <xsl:template match="o" mode="application">
    <xsl:param name="position" select="1"/>
    <xsl:value-of select="eo:comma($position)"/>
    <xsl:choose>
      <xsl:when test="@as">
        <xsl:value-of select="eo:specials(@as, true())"/>
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
