<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="remove-noise" version="2.0">
  <!--
  This stylesheet is used ONLY for testing purposes, to cleanup
  the XMIR document by removing all listings, metas, etc. Only
  objects will stay with the information relevant to processing of them.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object/listing">
    <!-- delete it -->
  </xsl:template>
  <xsl:template match="/object/sheets">
    <!-- delete it -->
  </xsl:template>
  <xsl:template match="xmir">
    <!-- delete it -->
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
