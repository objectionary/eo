<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="_stepped" version="2.0">
  <xsl:param name="step"/>
  <xsl:param name="sheet"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object/sheets">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:element name="sheet">
        <xsl:value-of select="$sheet"/>
      </xsl:element>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="/object[not(sheets)]">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <sheets>
        <xsl:element name="sheet">
          <xsl:value-of select="$sheet"/>
        </xsl:element>
      </sheets>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
