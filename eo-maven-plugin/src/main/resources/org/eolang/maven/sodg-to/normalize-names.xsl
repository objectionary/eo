<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="normalize-names" version="2.0">
  <!--
  This one renames all <a/> elements that start with '$Φ.'. All
  vertices get simple unique integer-based numbers, such as 'v42'.
  -->
  <xsl:import href="/org/eolang/maven/sodg/_macros.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="sorted">
    <xsl:perform-sort select="/object/sodg/i/a[starts-with(text(), '$Φ.')]">
      <xsl:sort select="."/>
    </xsl:perform-sort>
  </xsl:variable>
  <xsl:function name="eo:renamed">
    <xsl:param name="v" as="xs:string"/>
    <xsl:text>$v</xsl:text>
    <xsl:value-of select="index-of(distinct-values($sorted/a), $v)"/>
  </xsl:function>
  <xsl:template match="/object/sodg/i/a[starts-with(text(), '$Φ.')]" priority="1">
    <xsl:copy>
      <xsl:value-of select="eo:renamed(.)"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="/object/sodg/i/a[text() = 'ν0']" priority="1">
    <xsl:copy>
      <xsl:text>v0</xsl:text>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
