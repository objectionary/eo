<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="move-voids-up" version="2.0">
  <!--
  Move void attributes on top
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[count(o)&gt;0]">
    <xsl:apply-templates select="." mode="with-attrs"/>
  </xsl:template>
  <xsl:template match="o[count(o[eo:void(.)])&gt;0 and count(o[not(eo:void(.))])&gt;0]" mode="with-attrs">
    <xsl:apply-templates select="." mode="with-diff-attrs"/>
  </xsl:template>
  <xsl:template match="o[o[not(eo:void(.))]/following-sibling::o[eo:void(.)]]" mode="with-diff-attrs">
    <xsl:element name="o">
      <xsl:apply-templates select="@*"/>
      <xsl:for-each select="o[eo:idempotent(.)]">
        <xsl:copy-of select="."/>
      </xsl:for-each>
      <xsl:for-each select="o[eo:void(.)]">
        <xsl:copy-of select="."/>
      </xsl:for-each>
      <xsl:for-each select="o[not(eo:void(.)) and not(eo:idempotent(.))]">
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
