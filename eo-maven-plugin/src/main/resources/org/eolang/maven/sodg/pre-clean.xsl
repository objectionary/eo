<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="pre-clean" version="2.0">
  <!--
  Here we remove all the noise from XMIR, which we don't need
  during the further processing.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/listing">
    <!-- remove it -->
  </xsl:template>
  <xsl:template match="/program/sheets">
    <!-- remove it -->
  </xsl:template>
  <xsl:template match="/program/errors">
    <!-- remove it -->
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
