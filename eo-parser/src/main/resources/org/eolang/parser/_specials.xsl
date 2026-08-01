<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="_specials" version="2.0">
  <xsl:variable name="eo:cactoos" select="'🌵'"/>
  <xsl:variable name="eo:alpha" select="'α'"/>
  <xsl:variable name="eo:xi" select="'ξ'"/>
  <xsl:variable name="eo:phi" select="'φ'"/>
  <xsl:variable name="eo:rho" select="'ρ'"/>
  <xsl:variable name="eo:program" select="'Φ'"/>
  <xsl:variable name="eo:lambda" select="'λ'"/>
  <xsl:variable name="eo:empty" select="'∅'"/>
  <xsl:variable name="eo:bottom" select="'⊥'"/>
  <xsl:variable name="eo:space" select="' '"/>
  <!--
  What separates the type atoms of a void's "@type" annotation (R-3.4.8):
  the braces of a formation type, the single spaces between members and
  the trailing "?" of a maybe-⊥ union. The surface "|" is not among them —
  it never reaches XMIR, where members are space-separated.
  -->
  <xsl:variable name="eo:type-seps" select="'[{} ?]'"/>
  <xsl:variable name="eo:new-line" select="'&#10;'"/>
</xsl:stylesheet>
