<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="tuples-to-stars" version="2.0">
  <!--
    Performs the reverse operation of "/org/eolang/parser/stars-to-tuples.xsl"
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@base='Q.org.eolang.tuple.empty' and not(@star)]">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:attribute name="star"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[@base='Q.org.eolang.tuple.empty.with']">
    <xsl:element name="o">
      <xsl:attribute name="star"/>
      <xsl:attribute name="base" select="'Q.org.eolang.tuple'"/>
      <xsl:apply-templates select="@* except @base"/>
      <xsl:apply-templates select="node()"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="o[@base='.with' and o[1][@star]]">
    <xsl:element name="o">
      <xsl:attribute name="star"/>
      <xsl:attribute name="base" select="'Q.org.eolang.tuple'"/>
      <xsl:apply-templates select="@* except @base"/>
      <xsl:copy-of select="o[@star]/o"/>
      <xsl:copy-of select="o[not(@star)]"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
