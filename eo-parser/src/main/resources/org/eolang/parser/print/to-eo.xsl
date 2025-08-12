<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="to-eo" version="2.0">
  <!--
  This one maps XMIR to EO original syntax in straight notation.
  It's used in Xmir.java class.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:variable name="eol" select="'&#10;'"/>
  <xsl:variable name="auto" select="concat('a', $eo:cactoos)"/>
  <xsl:variable name="comment">
    <xsl:text># No comments.</xsl:text>
    <xsl:value-of select="$eol"/>
  </xsl:variable>
  <xsl:output method="text" encoding="UTF-8"/>
  <!-- PROGRAM -->
  <xsl:template match="object">
    <xsl:copy>
      <xsl:apply-templates select="sheets|metas"/>
      <xsl:copy-of select="o[1]"/>
      <eo>
        <xsl:apply-templates select="license"/>
        <xsl:apply-templates select="metas"/>
        <xsl:apply-templates select="o[1]"/>
      </eo>
    </xsl:copy>
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
      <xsl:value-of select="replace(replace(string(tail), 'Φ̇', 'QQ'), 'Φ', 'Q')"/>
    </xsl:if>
    <xsl:value-of select="$eol"/>
  </xsl:template>
  <!-- OBJECT, NOT FREE ATTRIBUTE -->
  <xsl:template match="o[not(eo:void(.)) and not(@name=$eo:lambda)]">
    <xsl:param name="indent" select="''"/>
    <xsl:if test="position()&gt;1 and parent::objects">
      <xsl:value-of select="$eol"/>
    </xsl:if>
    <xsl:value-of select="$indent"/>
    <xsl:apply-templates select="." mode="head">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="." mode="tail"/>
    <xsl:value-of select="$eol"/>
    <xsl:apply-templates select="o[not(eo:void(.))]">
      <xsl:with-param name="indent" select="concat('  ', $indent)"/>
    </xsl:apply-templates>
  </xsl:template>
  <!-- BASED -->
  <xsl:template match="o[@base and not(@base='ξ.xi🌵') and not(eo:has-data(.))]" mode="head">
    <xsl:choose>
      <!-- NOT OPTIMIZED TUPLE -->
      <xsl:when test="@star">
        <xsl:text>*</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="no-alphas" select="translate(@base, $eo:alpha, '~')"/>
        <xsl:choose>
          <xsl:when test="starts-with(@base, 'Φ̇.org.eolang.')">
            <xsl:value-of select="substring-after($no-alphas, 'Φ̇.org.eolang.')"/>
          </xsl:when>
          <xsl:when test="starts-with(@base, 'Φ.org.eolang.')">
            <xsl:value-of select="substring-after($no-alphas, 'Φ.org.eolang.')"/>
          </xsl:when>
          <xsl:when test="starts-with(@base, 'ξ.')">
            <xsl:choose>
              <xsl:when test="contains(@base, $eo:rho)">
                <xsl:text>^</xsl:text>
                <xsl:value-of select="substring-after($no-alphas, 'ξ.ρ')"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="substring-after($no-alphas, 'ξ.')"/>
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
    <xsl:param name="indent"/>
    <xsl:if test="@name and not(starts-with(@name, $auto))">
      <xsl:value-of select="$comment"/>
      <xsl:value-of select="$indent"/>
    </xsl:if>
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
        <xsl:text> ?</xsl:text>
      </xsl:if>
      <xsl:if test="eo:idempotent(.)">
        <xsl:text>'</xsl:text>
      </xsl:if>
    </xsl:if>
  </xsl:template>
  <!-- DATA -->
  <xsl:template match="o[eo:has-data(.)]" mode="head">
    <xsl:value-of select="eo:read-data(.)"/>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
