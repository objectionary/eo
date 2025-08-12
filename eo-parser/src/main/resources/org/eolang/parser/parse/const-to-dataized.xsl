<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="const-to-dataized" version="2.0">
  <!--
  Replace @const with dataized.as-bytes
  1. a > b!     => (dataized a).as-bytes > b
  2. x.y z > m! => (dataized (x.y z)).as-bytes > m
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- Generate unique name for an abstract object -->
  <xsl:function name="eo:unique-name">
    <xsl:param name="name"/>
    <xsl:param name="scope"/>
    <xsl:param name="counter"/>
    <xsl:variable name="unique">
      <xsl:choose>
        <xsl:when test="$counter=0">
          <xsl:value-of select="$name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="concat($name, '-', $counter)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$scope[o[@name=$unique]]">
        <xsl:value-of select="eo:unique-name($name, $scope, $counter + 1)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$unique"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- Template -->
  <xsl:template match="o[@const]">
    <xsl:element name="o">
      <xsl:attribute name="base" select="'.as-bytes'"/>
      <xsl:attribute name="name" select="@name"/>
      <xsl:attribute name="line" select="@line"/>
      <xsl:attribute name="pos" select="@pos + 8"/>
      <xsl:element name="o">
        <xsl:attribute name="base" select="'Φ.org.eolang.dataized'"/>
        <xsl:element name="o">
          <xsl:for-each select="@*[name()!='const' and name()!='name']">
            <xsl:attribute name="{name()}">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:for-each>
          <xsl:if test="eo:abstract(.)">
            <xsl:attribute name="name">
              <xsl:value-of select="eo:unique-name(@name, ./parent::o, 0)"/>
            </xsl:attribute>
          </xsl:if>
          <xsl:for-each select="o">
            <xsl:apply-templates select="."/>
          </xsl:for-each>
          <xsl:if test="eo:has-data(.)">
            <xsl:value-of select="."/>
          </xsl:if>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
