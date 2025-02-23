<?xml version="1.0"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="normalize-bytes" version="2.0">
  <!--
  Convert such XMIR:
    <o base='any'>
      <o as="0">01-2A</o>
    </o>
  to this:
    <o base='any'>
      01-2A
    </o>
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[eo:abstract(.) and @as='α0' and eo:has-data(.)]">
    <xsl:value-of select="text()"/>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
