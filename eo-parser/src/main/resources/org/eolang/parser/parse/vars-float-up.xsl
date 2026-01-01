<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="vars-float-up" version="2.0">
  <!--
  If we see this code, where a name is defined inside
  an abstract objects:

  [] > test
    hello
      foo > x
        15

  We move "x" declaration to the nearest abstract object
  and make it its attribute:

  [] > test
    foo > x
      15
    hello
      x
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="object">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()" mode="full"/>
      <xsl:for-each select="o/descendant::o[@name]">
        <xsl:if test="not(ancestor::o[eo:abstract(.)])">
          <xsl:apply-templates select="." mode="full"/>
        </xsl:if>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[eo:abstract(.) and not(../object)]" priority="0">
    <xsl:apply-templates select="." mode="full"/>
  </xsl:template>
  <xsl:template match="o[eo:abstract(.)]" mode="full" priority="1">
    <xsl:variable name="o" select="."/>
    <xsl:copy>
      <xsl:if test="not(@name) and @as">
        <xsl:attribute name="as" select="@as"/>
      </xsl:if>
      <xsl:apply-templates select="node()|@* except @as"/>
      <xsl:for-each select="o/descendant::o[@name]">
        <xsl:if test="ancestor::o[eo:abstract(.)][1]/generate-id() = generate-id($o)">
          <xsl:apply-templates select="." mode="full"/>
        </xsl:if>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[@name and ancestor::o[1][not(eo:abstract(.))]]">
    <xsl:element name="o">
      <xsl:attribute name="base">
        <xsl:value-of select="@name"/>
      </xsl:attribute>
      <xsl:apply-templates select="@line"/>
      <xsl:apply-templates select="@as"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
