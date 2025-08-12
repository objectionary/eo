<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="tuples-to-stars" version="2.0">
  <!--
    Performs the reverse operation of "/org/eolang/parser/stars-to-tuples.xsl"
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[(starts-with(@base, 'Φ.org.eolang.tuple') or starts-with(@base, 'Φ̇.org.eolang.tuple')) and o[1][starts-with(@base, 'Φ.org.eolang.tuple') or starts-with(@base, 'Φ̇.org.eolang.tuple')]]">
    <xsl:variable name="arg">
      <xsl:apply-templates select="o[2]"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:attribute name="star"/>
      <xsl:apply-templates select="o[1]" mode="inner"/>
      <xsl:apply-templates select="$arg" mode="no-as"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o" mode="inner">
    <xsl:if test="starts-with(@base, 'Φ.org.eolang.tuple') or starts-with(@base, 'Φ̇.org.eolang.tuple')">
      <xsl:variable name="arg">
        <xsl:apply-templates select="o[2]"/>
      </xsl:variable>
      <xsl:apply-templates select="o[1]" mode="inner"/>
      <xsl:apply-templates select="$arg" mode="no-as"/>
    </xsl:if>
  </xsl:template>
  <xsl:template match="*" mode="no-as">
    <xsl:copy>
      <xsl:copy-of select="@* except @as"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
