<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="catch-duplicate-edges" version="2.0">
  <!--
  Here we go through all vertices and confirm that they don't have
  duplicate edges (with the same label).
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/graph/v/e[preceding-sibling::e/@title = @title]">
    <xsl:variable name="e" select="."/>
    <xsl:message terminate="yes">
      <xsl:text>The edge </xsl:text>
      <xsl:value-of select="$e/@id"/>
      <xsl:text> labeled as '</xsl:text>
      <xsl:value-of select="$e/@title"/>
      <xsl:text>' is a duplicate of another edge with the same label</xsl:text>
    </xsl:message>
  </xsl:template>
  <xsl:template match="/graph/v/e[preceding-sibling::e/@id = @id]">
    <xsl:variable name="e" select="."/>
    <xsl:message terminate="yes">
      <xsl:text>The edge </xsl:text>
      <xsl:value-of select="$e/@id"/>
      <xsl:text>' is a duplicate of another edge with the same @id</xsl:text>
    </xsl:message>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
