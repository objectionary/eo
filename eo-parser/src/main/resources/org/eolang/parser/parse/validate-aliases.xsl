<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  id="validate-aliases" version="2.0">
  <!--
  Here we check that every alias declared with +alias
  is actually used somewhere in the file. If an alias
  is declared but never referenced, we add an error.
  Aliases may be in short form (+alias org.example.foo)
  or long form (+alias foo org.example.foo), so we check
  both part[1] and the last segment of the FQN.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object/errors">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:apply-templates select="/object/metas/meta[head='alias']" mode="check"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="/object[not(errors)]">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:variable name="errs">
        <xsl:apply-templates select="metas/meta[head='alias']" mode="check"/>
      </xsl:variable>
      <xsl:if test="$errs/*">
        <errors>
          <xsl:copy-of select="$errs/*"/>
        </errors>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="meta[head='alias']" mode="check">
    <xsl:variable name="part1" select="part[1]/text()"/>
    <xsl:variable name="last-segment" select="tokenize($part1, '\.')[last()]"/>
    <xsl:if test="not(//o[@base = $part1 or @base = $last-segment])">
      <xsl:element name="error">
        <xsl:attribute name="check">validate-aliases</xsl:attribute>
        <xsl:attribute name="line"><xsl:value-of select="@line"/></xsl:attribute>
        <xsl:attribute name="severity">error</xsl:attribute>
        <xsl:text>Alias '</xsl:text>
        <xsl:value-of select="$last-segment"/>
        <xsl:text>' is declared but never used</xsl:text>
      </xsl:element>
    </xsl:if>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
