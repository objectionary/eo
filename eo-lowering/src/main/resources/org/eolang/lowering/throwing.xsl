<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="throwing" version="3.0">
  <!--
  Here every "T" of one XMIR file becomes a copy of "throw", the formation
  the lowering keeps in "Φ.l/" next to the mark and the root, and the
  argument of the "T" becomes the "message" of that copy. The parser writes
  a "T" as the terminator "⊥" of the calculus, and phino stops at "⊥" with
  nothing left of the message, so an error of EO could never reach the Java
  of an atom. A copy of "throw" carries its message instead, and phino stops
  at the λ of "throw", which no rule answers, with the message right there
  in its protocol. Every other node is copied as it is.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@base = '⊥']/@base">
    <xsl:attribute name="base" select="'Φ.l/.throw'"/>
  </xsl:template>
  <xsl:template match="o[@base = '⊥']/o[@as = 'α0']/@as">
    <xsl:attribute name="as" select="'message'"/>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
