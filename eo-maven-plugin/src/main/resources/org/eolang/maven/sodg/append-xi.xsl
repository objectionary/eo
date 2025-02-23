<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="append-xi" version="2.0">
  <!--
  Here we find all objects that have @base as '^' and
  convert them to '$.^' respectively, by adding new
  <o> elements.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[not(@level) and @base='^']">
    <xsl:copy>
      <xsl:apply-templates select="node()|@* except @base"/>
      <xsl:attribute name="base">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="./@base"/>
      </xsl:attribute>
      <o base="$">
        <xsl:attribute name="loc">
          <xsl:value-of select="./@loc"/>
          <xsl:text>.ρ</xsl:text>
        </xsl:attribute>
      </o>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
