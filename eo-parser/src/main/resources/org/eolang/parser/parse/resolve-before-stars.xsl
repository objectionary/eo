<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="resolve-before-stars" version="2.0">
  <!--
    Converts such XMIR with @before-star attributes:

    <o base="seq" before-star="2">
      <o base="1".../>
      <o base="2".../>
      <o base="3".../>
      <o base="4".../>
    </o>

    Into the next one:

    <o base="seq">
      <o base="1".../>
      <o base="2".../>
      <o base="tuple" star="">
        <o base="3".../>
        <o base="4".../>
      </o>
    </o>
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@before-star]">
    <xsl:variable name="before" select="@before-star" as="xs:int"/>
    <xsl:element name="o">
      <xsl:for-each select="@* except @before-star">
        <xsl:attribute name="{name()}" select="."/>
      </xsl:for-each>
      <xsl:for-each select="o[position() &lt;= $before]">
        <xsl:apply-templates select="."/>
      </xsl:for-each>
      <xsl:element name="o">
        <xsl:attribute name="base" select="'Φ.org.eolang.tuple'"/>
        <xsl:attribute name="star"/>
        <xsl:for-each select="o[position() &gt; $before]">
          <xsl:apply-templates select="."/>
        </xsl:for-each>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
