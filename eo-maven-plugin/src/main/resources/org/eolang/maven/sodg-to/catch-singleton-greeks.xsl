<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="catch-singleton-greeks" version="2.0">
  <!--
  Here we catch vertices that have "epsilon" departing edges and
  some other edges in addition to them. We don't want this to happen,
  since "epsilon" must always be alone.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="greeks" select="('ε', 'γ')"/>
  <xsl:template match="/graph/v">
    <xsl:variable name="v" select="."/>
    <xsl:for-each select="$greeks">
      <xsl:variable name="letter" select="."/>
      <xsl:if test="$v/e[@title=$letter] and $v/e[@title!=$letter]">
        <xsl:message terminate="yes">
          <xsl:text>The edge departing from '</xsl:text>
          <xsl:value-of select="$v/@id"/>
          <xsl:text>' must be alone, since it's labeled as '</xsl:text>
          <xsl:value-of select="$letter"/>
          <xsl:text>'</xsl:text>
        </xsl:message>
      </xsl:if>
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
