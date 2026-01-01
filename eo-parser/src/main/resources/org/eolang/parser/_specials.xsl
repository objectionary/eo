<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="_specials" version="2.0">
  <xsl:variable name="eo:cactoos" select="'🌵'"/>
  <xsl:variable name="eo:alpha" select="'α'"/>
  <xsl:variable name="eo:xi" select="'ξ'"/>
  <xsl:variable name="eo:delta" select="'Δ'"/>
  <xsl:variable name="eo:phi" select="'φ'"/>
  <xsl:variable name="eo:rho" select="'ρ'"/>
  <xsl:variable name="eo:program" select="'Φ'"/>
  <xsl:variable name="eo:def-package" select="'Φ̇'"/>
  <xsl:variable name="eo:lambda" select="'λ'"/>
  <xsl:variable name="eo:arrow">
    <xsl:value-of select="$eo:space"/>
    <xsl:text>↦</xsl:text>
    <xsl:value-of select="$eo:space"/>
  </xsl:variable>
  <xsl:variable name="eo:dashed-arrow">
    <xsl:value-of select="$eo:space"/>
    <xsl:text>⤍</xsl:text>
    <xsl:value-of select="$eo:space"/>
  </xsl:variable>
  <xsl:variable name="eo:lb" select="'⟦'"/>
  <xsl:variable name="eo:rb" select="'⟧'"/>
  <xsl:variable name="eo:clb" select="'('"/>
  <xsl:variable name="eo:crb" select="')'"/>
  <xsl:variable name="eo:empty" select="'∅'"/>
  <xsl:variable name="eo:space" select="' '"/>
  <xsl:variable name="eo:new-line" select="'&#10;'"/>
</xsl:stylesheet>
