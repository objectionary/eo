<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="catch-conflicting-greeks" version="2.0">
  <!--
  Here we catch vertices that have departing edges labeled with greek letters
  conflicting with each other, for example "lambda" and "epsilon" at the same time.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/graph/v">
    <xsl:variable name="greeks" select="('λ', 'β', 'ε', 'π', 'γ')"/>
    <xsl:variable name="v" select="."/>
    <xsl:for-each select="$greeks">
      <xsl:variable name="left" select="."/>
      <xsl:for-each select="$greeks">
        <xsl:variable name="right" select="."/>
        <xsl:if test="$left != $right and $v/e[@title=$left] and $v/e[@title=$right]">
          <xsl:message terminate="yes">
            <xsl:text>The edges departing from '</xsl:text>
            <xsl:value-of select="$v/@id"/>
            <xsl:text>' conflict with each other, we can't have '</xsl:text>
            <xsl:value-of select="$left"/>
            <xsl:text>' and '</xsl:text>
            <xsl:value-of select="$right"/>
            <xsl:text>' at the same time</xsl:text>
          </xsl:message>
        </xsl:if>
      </xsl:for-each>
    </xsl:for-each>
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
