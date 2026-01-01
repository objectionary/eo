<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="anonymous-to-nested" version="2.0">
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="class[descendant::o[not(@base) and not(@name) and o]]">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:for-each select=".//o[not(@base) and not(@name) and o]">
        <xsl:element name="nested">
          <xsl:apply-templates select="node()|@*"/>
        </xsl:element>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
