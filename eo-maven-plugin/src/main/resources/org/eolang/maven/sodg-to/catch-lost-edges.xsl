<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="catch-lost-edges" version="2.0">
  <!--
  Here we go through all edges and confirm that they have
  relative vertices. We don't want to have an edge that departures
  from a vertex but doesn't arrive anywhere.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/graph/v/e">
    <xsl:variable name="e" select="."/>
    <xsl:if test="not(/graph/v[@id=$e/@to])">
      <xsl:message terminate="yes">
        <xsl:text>The edge departs from '</xsl:text>
        <xsl:value-of select="$e/parent::v/@id"/>
        <xsl:text>' and points to the vertex '</xsl:text>
        <xsl:value-of select="$e/@to"/>
        <xsl:text>'; however the target vertex doesn't exist in the graph</xsl:text>
      </xsl:message>
    </xsl:if>
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
