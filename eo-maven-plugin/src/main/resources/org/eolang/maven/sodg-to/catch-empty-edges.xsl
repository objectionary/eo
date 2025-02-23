<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="catch-empty-edges" version="2.0">
  <!--
  Here we go through all edges and confirm that they have
  proper labels, not empty.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/graph/v/e[@title = '']">
    <xsl:variable name="e" select="."/>
    <xsl:message terminate="yes">
      <xsl:text>The edge that departs from '</xsl:text>
      <xsl:value-of select="$e/parent::v/@id"/>
      <xsl:text>' and points to '</xsl:text>
      <xsl:value-of select="$e/@to"/>
      <xsl:text>' has a empty label</xsl:text>
    </xsl:message>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
