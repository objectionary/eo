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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="to-salty-phi" version="2.0">
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="text"/>
  <!-- Variables -->
  <xsl:variable name="aliases" select="program/metas/meta/part[last()]"/>
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
  <xsl:variable name="program">
    <select>Φ</select>
  </xsl:variable>
  <xsl:variable name="lambda">
    <select>λ</select>
  </xsl:variable>
  <xsl:variable name="arrow">
    <xsl:value-of select="$space"/>
    <select>↦</select>
    <xsl:value-of select="$space"/>
  </xsl:variable>
  <xsl:variable name="dashed-arrow">
    <xsl:value-of select="$space"/>
    <select>⤍</select>
    <xsl:value-of select="$space"/>
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
  <xsl:variable name="space">
    <xsl:text> </xsl:text>
  </xsl:variable>
  <!-- Functions -->
  <!-- ADD XI OR NOT -->
  <xsl:function name="eo:add-xi">
    <xsl:param name="add"/>
    <xsl:if test="$add">
      <xsl:value-of select="$xi"/>
      <xsl:text>.</xsl:text>
    </xsl:if>
  </xsl:function>
  <!-- Get clean escaped object name  -->
  <xsl:function name="eo:lambda-name">
    <xsl:param name="name"/>
    <xsl:value-of select="concat('L', replace(replace(replace(substring($name, 3), '\.', '_'), '-', '_'), '@', 'φ'))"/>
  </xsl:function>
  <!-- SPECIAL CHARACTERS -->
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
      <xsl:when test="$n='$'">
        <xsl:value-of select="$xi"/>
      </xsl:when>
      <xsl:when test="$n='Q'">
        <xsl:value-of select="$program"/>
      </xsl:when>
      <xsl:when test="starts-with($n, 'org.eolang')">
        <xsl:value-of select="$program"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$n"/>
      </xsl:when>
      <xsl:when test="$aliases[text()=$n]">
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
  <!-- COMMA WITH SPACE -->
  <xsl:function name="eo:comma">
    <xsl:param name="pos"/>
    <xsl:param name="tabs"/>
    <xsl:if test="$pos&gt;1">
      <xsl:text>,</xsl:text>
      <xsl:value-of select="eo:eol($tabs)"/>
    </xsl:if>
  </xsl:function>
  <!-- EOL WITH INDENTATION -->
  <xsl:function name="eo:eol">
    <xsl:param name="tabs"/>
    <xsl:value-of select="'&#10;'"/>
    <xsl:for-each select="1 to $tabs">
      <xsl:text>  </xsl:text>
    </xsl:for-each>
  </xsl:function>
  <!-- Program -->
  <xsl:template match="program">
    <program>
      <xsl:copy-of select="./sheets"/>
      <xsl:copy-of select="./errors"/>
      <phi>
        <xsl:text>{</xsl:text>
        <xsl:variable name="tabs" select="2"/>
        <xsl:value-of select="eo:eol(1)"/>
        <xsl:value-of select="$lb"/>
        <xsl:value-of select="eo:eol(2)"/>
        <xsl:variable name="has-package" select="metas/meta/head[text()='package']"/>
        <xsl:variable name="package" select="metas/meta[head[text()='package']]/tail[1]"/>
        <xsl:variable name="parts" select="tokenize($package,'\.')"/>
        <xsl:variable name="length" select="string-length($package)-string-length(replace($package,'\.',''))"/>
        <xsl:choose>
          <xsl:when test="$has-package">
            <xsl:for-each select="$parts">
              <xsl:value-of select="."/>
              <xsl:value-of select="$arrow"/>
              <xsl:value-of select="$lb"/>
              <xsl:value-of select="eo:eol($tabs+position())"/>
            </xsl:for-each>
            <xsl:apply-templates select="objects">
              <xsl:with-param name="tabs" select="$tabs + $length + 1"/>
              <xsl:with-param name="package">
                <xsl:value-of select="$program"/>
                <xsl:text>.</xsl:text>
                <xsl:value-of select="$package"/>
              </xsl:with-param>
            </xsl:apply-templates>
            <xsl:for-each select="$parts">
              <xsl:value-of select="eo:comma(2, $tabs + $length + 2 - position())"/>
              <xsl:value-of select="$lambda"/>
              <xsl:value-of select="$dashed-arrow"/>
              <xsl:text>Package</xsl:text>
              <xsl:value-of select="eo:eol($tabs + $length + 1 - position())"/>
              <xsl:value-of select="$rb"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="objects">
              <xsl:with-param name="tabs" select="$tabs"/>
              <xsl:with-param name="package" select="$program"/>
            </xsl:apply-templates>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:value-of select="eo:eol(1)"/>
        <xsl:value-of select="$rb"/>
        <xsl:value-of select="eo:eol(0)"/>
        <xsl:text>}</xsl:text>
      </phi>
    </program>
  </xsl:template>
  <!-- Objects  -->
  <xsl:template match="objects">
    <xsl:param name="tabs"/>
    <xsl:param name="package"/>
    <xsl:for-each select="o">
      <xsl:value-of select="eo:comma(position(), $tabs)"/>
      <xsl:apply-templates select=".">
        <xsl:with-param name="tabs" select="$tabs"/>
        <xsl:with-param name="package" select="$package"/>
      </xsl:apply-templates>
    </xsl:for-each>
  </xsl:template>
  <!-- Void attribute -->
  <xsl:template match="o[eo:void(.)]">
    <xsl:value-of select="eo:specials(@name, true())"/>
    <xsl:value-of select="$arrow"/>
    <xsl:value-of select="$empty"/>
  </xsl:template>
  <!-- Find path template -->
  <xsl:template match="*" mode="path">
    <xsl:param name="find"/>
    <xsl:variable name="parent" select="parent::*"/>
    <xsl:variable name="rho-dot">
      <xsl:value-of select="eo:specials('^', true())"/>
      <xsl:text>.</xsl:text>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="eo:abstract($parent)">
        <xsl:if test="not($parent/o[@name=$find])">
          <xsl:value-of select="$rho-dot"/>
          <xsl:apply-templates select="$parent" mode="path">
            <xsl:with-param name="find" select="$find"/>
          </xsl:apply-templates>
        </xsl:if>
      </xsl:when>
      <xsl:when test="not($parent[name()='objects'])">
        <xsl:apply-templates select="$parent" mode="path">
          <xsl:with-param name="find" select="$find"/>
        </xsl:apply-templates>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <!-- Just object -->
  <xsl:template match="o[@base and not(eo:void(.))]">
    <xsl:param name="tabs"/>
    <xsl:param name="package"/>
    <xsl:if test="@name">
      <xsl:value-of select="eo:specials(@name, true())"/>
      <xsl:value-of select="$arrow"/>
    </xsl:if>
    <xsl:choose>
      <!-- Not method -->
      <xsl:when test="not(starts-with(@base, '.'))">
        <xsl:choose>
          <xsl:when test="@ref and not(eo:has-data(.))">
            <xsl:value-of select="eo:add-xi(true())"/>
            <xsl:apply-templates select="." mode="path">
              <xsl:with-param name="find" select="@base"/>
            </xsl:apply-templates>
            <xsl:value-of select="eo:specials(@base, true())"/>
          </xsl:when>
          <xsl:when test="eo:has-data(.) and (@base='org.eolang.number' or @base='org.eolang.string')">
            <xsl:value-of select="text()"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="eo:specials(@base, false())"/>
          </xsl:otherwise>
        </xsl:choose>
        <!-- Nested objects -->
        <xsl:if test="count(o)&gt;0">
          <xsl:text>(</xsl:text>
          <xsl:value-of select="eo:eol($tabs+1)"/>
          <xsl:for-each select="o">
            <xsl:apply-templates select="." mode="application">
              <xsl:with-param name="position" select="position()"/>
              <xsl:with-param name="tabs" select="$tabs+1"/>
            </xsl:apply-templates>
          </xsl:for-each>
          <xsl:value-of select="eo:eol($tabs)"/>
          <xsl:text>)</xsl:text>
        </xsl:if>
      </xsl:when>
      <!-- Method -->
      <xsl:otherwise>
        <xsl:apply-templates select="o[position()=1]">
          <xsl:with-param name="tabs" select="$tabs"/>
          <xsl:with-param name="package" select="$package"/>
        </xsl:apply-templates>
        <xsl:value-of select="eo:specials(@base, true())"/>
        <!-- Nested objects -->
        <xsl:if test="count(o)&gt;1">
          <xsl:text>(</xsl:text>
          <xsl:value-of select="eo:eol($tabs+1)"/>
          <xsl:for-each select="o[position()!=1]">
            <xsl:apply-templates select="." mode="application">
              <xsl:with-param name="position" select="position()"/>
              <xsl:with-param name="tabs" select="$tabs+1"/>
            </xsl:apply-templates>
          </xsl:for-each>
          <xsl:value-of select="eo:eol($tabs)"/>
          <xsl:text>)</xsl:text>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
    <!-- Data -->
    <xsl:if test="eo:has-data(.) and @base!='org.eolang.number' and @base!='org.eolang.string'">
      <xsl:text>(</xsl:text>
      <xsl:value-of select="eo:eol($tabs+1)"/>
      <xsl:value-of select="$alpha"/>
      <xsl:text>0</xsl:text>
      <xsl:value-of select="$arrow"/>
      <xsl:value-of select="$lb"/>
      <xsl:value-of select="$space"/>
      <xsl:value-of select="$delta"/>
      <xsl:value-of select="$dashed-arrow"/>
      <xsl:value-of select="text()[last()]"/>
      <xsl:value-of select="$space"/>
      <xsl:value-of select="$rb"/>
      <xsl:value-of select="eo:eol($tabs)"/>
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>
  <!-- Formation -->
  <xsl:template match="o[eo:abstract(.)]">
    <xsl:param name="tabs"/>
    <xsl:param name="package"/>
    <xsl:variable name="name" select="eo:specials(@name, true())"/>
    <xsl:if test="@name">
      <xsl:value-of select="$name"/>
      <xsl:value-of select="$arrow"/>
    </xsl:if>
    <xsl:value-of select="$lb"/>
    <!-- Atom or not empty formation -->
    <xsl:if test="@atom or count(o)&gt;0">
      <xsl:value-of select="eo:eol($tabs+1)"/>
      <!-- Atom -->
      <xsl:if test="@atom">
        <xsl:variable name="lambda-name">
          <xsl:value-of select="$package"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$name"/>
        </xsl:variable>
        <xsl:value-of select="$lambda"/>
        <xsl:value-of select="$dashed-arrow"/>
        <xsl:value-of select="eo:lambda-name($lambda-name)"/>
        <xsl:if test="count(o)&gt;0">
          <xsl:value-of select="eo:comma(2, $tabs+1)"/>
        </xsl:if>
      </xsl:if>
      <!-- Inner objects -->
      <xsl:for-each select="o">
        <xsl:value-of select="eo:comma(position(), $tabs+1)"/>
        <xsl:apply-templates select=".">
          <xsl:with-param name="tabs" select="$tabs+1"/>
          <xsl:with-param name="package">
            <xsl:value-of select="$package"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="$name"/>
          </xsl:with-param>
        </xsl:apply-templates>
      </xsl:for-each>
      <xsl:value-of select="eo:eol($tabs)"/>
    </xsl:if>
    <xsl:value-of select="$rb"/>
  </xsl:template>
  <!-- Application -->
  <xsl:template match="o" mode="application">
    <xsl:param name="tabs"/>
    <xsl:param name="position" select="1"/>
    <xsl:value-of select="eo:comma($position, $tabs)"/>
    <xsl:choose>
      <xsl:when test="@as">
        <xsl:if test="matches(@as,'^[0-9][1-9]*$')">
          <xsl:value-of select="$alpha"/>
        </xsl:if>
        <xsl:value-of select="eo:specials(@as, true())"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$alpha"/>
        <xsl:value-of select="$position - 1"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="$arrow"/>
    <xsl:apply-templates select=".">
      <xsl:with-param name="tabs" select="$tabs"/>
    </xsl:apply-templates>
  </xsl:template>
  <!-- Ignore other elements -->
  <xsl:template match="node()|@*">
    <xsl:apply-templates/>
  </xsl:template>
</xsl:stylesheet>
