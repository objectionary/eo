<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="add-loc-to-objects" version="2.0">
  <!--
  Here we add @loc attribute to the "objects" element in XML.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/objects">
    <xsl:copy>
      <xsl:attribute name="loc">
        <xsl:text>Φ</xsl:text>
      </xsl:attribute>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
