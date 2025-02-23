<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="normalize-attrs" version="2.0">
  <!--
  This one renames all attributes that start with '$Φ.'. All
  vertices get simple unique integer-based numbers, such as 'v42'.
  -->
  <xsl:import href="/org/eolang/maven/sodg/_macros.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="@*[starts-with(., '$Φ.')]" priority="1">
    <xsl:attribute name="{name()}">
      <xsl:text>$v</xsl:text>
      <xsl:value-of select="index-of(distinct-values(//@*[starts-with(., '$Φ.')]), .)"/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@*[.='ν0']" priority="1">
    <xsl:attribute name="{name()}">
      <xsl:text>v0</xsl:text>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
