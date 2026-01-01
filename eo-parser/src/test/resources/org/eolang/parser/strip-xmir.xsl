<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="strip-xmir" version="2.0">
  <!--
  This one removes all 'meaningless' elements from XMIR. We
  use this one to compare two XMIR documents for semantic
  equivalence.
  -->
  <xsl:output method="xml" encoding="UTF-8" indent="yes"/>
  <xsl:strip-space elements="*"/>
  <xsl:template match="/object/@*">
    <!-- Program attributes are not important -->
  </xsl:template>
  <xsl:template match="/object/listing">
    <!-- Not important -->
  </xsl:template>
  <xsl:template match="/object/errors">
    <!-- Not important -->
  </xsl:template>
  <xsl:template match="/object/sheets">
    <!-- Not important -->
  </xsl:template>
  <xsl:template match="@line">
    <!-- Not important -->
  </xsl:template>
  <xsl:template match="@pos">
    <!-- Not important -->
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
