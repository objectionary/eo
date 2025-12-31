<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="locals-to-aliases" version="2.0">
  <xsl:param name="name"/>
  <xsl:param name="reserved"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="pkg" select="normalize-space(/object/metas/meta[head='package'][1]/tail)"/>
  <xsl:variable name="qqs" select="tokenize(normalize-space($reserved), '[\s,]+')"/>
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="@base[starts-with(., 'Φ.') and not(. = $qqs)]">
    <xsl:variable name="seg" select="tokenize(., '\.')[last()]"/>
    <xsl:attribute name="base" select="concat($pkg, '.', $seg)"/>
  </xsl:template>
  <xsl:template match="metas">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
      <xsl:for-each-group select="//@base[starts-with(., 'Φ.') and not(. = $qqs)]" group-by="tokenize(., '\.')[last()]">
        <xsl:variable name="seg"  select="current-grouping-key()"/>
        <xsl:variable name="new"  select="concat($pkg, '.', $seg)"/>
        <xsl:variable name="tail" select="concat($seg, ' ', $new)"/>
        <xsl:if test="not(/object/metas/meta[head='alias' and tail=$new])">
          <meta>
            <head>alias</head>
            <tail><xsl:value-of select="$tail"/></tail>
            <part><xsl:value-of select="$seg"/></part>
            <part><xsl:value-of select="$new"/></part>
          </meta>
        </xsl:if>
      </xsl:for-each-group>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
