<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="to-eo-tree" version="2.0">
  <!--
  This one maps XMIR to an intermediate "line tree" that is later
  laid out into pretty EO source by the Pretty class (penalty-based
  formatting). Each object becomes a <line> element carrying its
  rendered head ("base" attribute) and suffix ("tail" attribute),
  with nested <line> children for its (non-void) attributes. The
  layout engine decides, per node, whether to render it horizontally
  (inline) or vertically (indented), picking the option with the
  lowest penalty.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:variable name="eol" select="'&#10;'"/>
  <xsl:output method="xml" encoding="UTF-8"/>
  <!-- PROGRAM -->
  <xsl:template match="object">
    <object>
      <eo>
        <preamble>
          <xsl:apply-templates select="comments"/>
          <xsl:apply-templates select="license"/>
          <xsl:apply-templates select="metas"/>
        </preamble>
        <xsl:apply-templates select="o[1]" mode="tree"/>
      </eo>
    </object>
  </xsl:template>
  <!-- TOP COMMENT BLOCK -->
  <xsl:template match="comments">
    <xsl:for-each select="comment">
      <xsl:for-each select="tokenize(., $eol)">
        <xsl:choose>
          <xsl:when test=". = ''">
            <xsl:text>#</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text># </xsl:text>
            <xsl:value-of select="."/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:value-of select="$eol"/>
      </xsl:for-each>
    </xsl:for-each>
    <xsl:if test="comment">
      <xsl:value-of select="$eol"/>
    </xsl:if>
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
      <xsl:value-of select="replace(string(tail), 'Φ', 'Q')"/>
    </xsl:if>
    <xsl:value-of select="$eol"/>
  </xsl:template>
  <!-- OBJECT, NOT FREE ATTRIBUTE -->
  <xsl:template match="o[not(eo:void(.)) and not(@name=$eo:lambda)]" mode="tree">
    <line>
      <xsl:attribute name="base">
        <xsl:apply-templates select="." mode="head"/>
      </xsl:attribute>
      <xsl:attribute name="tail">
        <xsl:apply-templates select="." mode="tail"/>
      </xsl:attribute>
      <xsl:attribute name="abstract">
        <xsl:value-of select="if (eo:abstract(.) and not(eo:has-data(.))) then 'yes' else 'no'"/>
      </xsl:attribute>
      <xsl:apply-templates select="o[not(eo:void(.))]" mode="tree"/>
    </line>
  </xsl:template>
  <!-- BASED -->
  <xsl:template match="o[@base and not(eo:has-data(.))]" mode="head">
    <xsl:choose>
      <!-- NOT OPTIMIZED TUPLE -->
      <xsl:when test="@star">
        <xsl:text>*</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="@base=$eo:bottom">
            <xsl:text>T</xsl:text>
          </xsl:when>
          <xsl:when test="starts-with(@base, 'Φ.')">
            <xsl:value-of select="substring-after(@base, 'Φ.')"/>
          </xsl:when>
          <xsl:when test="starts-with(@base, 'ξ.')">
            <xsl:choose>
              <xsl:when test="contains(@base, $eo:rho)">
                <xsl:text>^</xsl:text>
                <xsl:value-of select="substring-after(@base, 'ξ.ρ')"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="substring-after(@base, 'ξ.')"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>
          <xsl:when test="starts-with(@base, '.')">
            <xsl:value-of select="substring(@base, 2)"/>
            <xsl:text>.</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="@base"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- ABSTRACT OR ATOM -->
  <xsl:template match="o[eo:abstract(.) and not(eo:has-data(.))]" mode="head">
    <xsl:text>[</xsl:text>
    <xsl:for-each select="o[eo:void(.)]">
      <xsl:if test="position()&gt;1">
        <xsl:text> </xsl:text>
      </xsl:if>
      <xsl:value-of select="@name"/>
    </xsl:for-each>
    <xsl:text>]</xsl:text>
  </xsl:template>
  <!-- TAIL: SUFFIX, NAME, CONST, ATOM -->
  <xsl:template match="o" mode="tail">
    <xsl:if test="@as">
      <xsl:text>:</xsl:text>
      <xsl:choose>
        <xsl:when test="starts-with(@as, $eo:alpha)">
          <xsl:value-of select="substring-after(@as, $eo:alpha)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@as"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:if test="@name">
      <xsl:choose>
        <xsl:when test="starts-with(@name, concat('a', $eo:cactoos))">
          <xsl:text> &gt;&gt;</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text> &gt; </xsl:text>
          <xsl:choose>
            <xsl:when test="@name = $eo:phi">
              <xsl:value-of select="'@'"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="@name"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="@const">
        <xsl:text>!</xsl:text>
      </xsl:if>
      <xsl:if test="eo:atom(.)">
        <xsl:text> /</xsl:text>
        <xsl:variable name="lambda-atom" select="string(./o[@name=$eo:lambda]/@atom)"/>
        <xsl:choose>
          <xsl:when test="starts-with($lambda-atom, 'Φ.')">
            <xsl:value-of select="substring-after($lambda-atom, 'Φ.')"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$lambda-atom"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:if>
    </xsl:if>
  </xsl:template>
  <!-- DATA -->
  <xsl:template match="o[eo:has-data(.)]" mode="head">
    <xsl:value-of select="eo:read-data(.)"/>
  </xsl:template>
</xsl:stylesheet>
