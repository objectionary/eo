<?xml version="1.0"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="wrap-bytes" version="2.0">
  <!--
  Convert such expression in XMIR:
    x ↦ ⟦ Δ ⤍ 01- ⟧,
  to this:
    x ↦ Φ.org.eolang.bytes( α0 ↦ ⟦ Δ ⤍ 01- ⟧ )
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[eo:abstract(.) and @name and eo:has-data(.)]">
    <xsl:element name="o">
      <xsl:attribute name="base">Q.org.eolang.bytes</xsl:attribute>
      <xsl:attribute name="name" select="@name"/>
      <xsl:element name="o">
        <xsl:attribute name="as" select="'α0'"/>
        <xsl:value-of select="text()"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
