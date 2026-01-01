<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="data" version="2.0">
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[o[1][eo:has-data(.)]]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:element name="value">
        <xsl:text>new byte[] {</xsl:text>
        <xsl:for-each select="tokenize(o[1]/text(), '-')">
          <xsl:if test=".!=''">
            <xsl:if test="position() &gt; 1">
              <xsl:text>, </xsl:text>
            </xsl:if>
            <xsl:text>(byte) 0x</xsl:text>
            <xsl:value-of select="."/>
          </xsl:if>
        </xsl:for-each>
        <xsl:text>}</xsl:text>
      </xsl:element>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
