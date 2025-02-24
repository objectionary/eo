<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="to-text" version="2.0">
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="EOL">
    <xsl:value-of select="'&#10;'"/>
  </xsl:variable>
  <xsl:template match="/sodg">
    <xsl:element name="text">
      <xsl:apply-templates select="i"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="i[@name='COMMENT']">
    <xsl:value-of select="$EOL"/>
    <xsl:text># </xsl:text>
    <xsl:value-of select="c"/>
    <xsl:value-of select="$EOL"/>
  </xsl:template>
  <xsl:template match="i[@name!='COMMENT']">
    <xsl:value-of select="@name"/>
    <xsl:text>(</xsl:text>
    <xsl:for-each select="a">
      <xsl:if test="position() &gt; 1">
        <xsl:text>, </xsl:text>
      </xsl:if>
      <xsl:value-of select="."/>
    </xsl:for-each>
    <xsl:text>);</xsl:text>
    <xsl:if test="c and not(empty(c/text()))">
      <xsl:text> # </xsl:text>
      <xsl:value-of select="c"/>
    </xsl:if>
    <xsl:value-of select="$EOL"/>
  </xsl:template>
</xsl:stylesheet>
