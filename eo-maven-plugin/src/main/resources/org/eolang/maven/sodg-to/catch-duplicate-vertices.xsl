<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="catch-duplicate-vertices" version="2.0">
  <!--
  Here we go through all vertices and confirm that they don't have
  duplicate IDs.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/graph/v[count(@id=/graph/v/@id) &gt; 1]">
    <xsl:variable name="v" select="."/>
    <xsl:message terminate="yes">
      <xsl:text>The vertex </xsl:text>
      <xsl:value-of select="$v/@id"/>
      <xsl:text> is a duplicate</xsl:text>
    </xsl:message>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
