<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="const-to-dataized" version="2.0">
  <!--
  Replace @const with dataized.as-bytes
  1. a > b!     => (dataized a).as-bytes > b
  2. x.y z > m! => (dataized (x.y z)).as-bytes > m
  3. 42.plus a! => a nameless const argument; the wrapper gets a cactus
  auto-name (as AutoName does in Java) so vars-float-up floats it up and
  leaves a proper reference, not an empty name that fails to round-trip.
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
    <!--
    The name to carry onto the floated .as-bytes wrapper. A const on a
    named binding (a > b!) already has one; a const on an anonymous
    inline argument (42.plus a!) has none, so we synthesise a cactus
    auto-name from the source coordinates (mirrors AutoName in Java).
    -->
    <xsl:variable name="cname">
      <xsl:choose>
        <xsl:when test="@name and @name != ''">
          <xsl:value-of select="@name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="concat('a🌵', @line, '-', @pos)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:element name="o">
      <xsl:attribute name="base" select="'.as-bytes'"/>
      <xsl:attribute name="name" select="$cname"/>
      <xsl:attribute name="line" select="@line"/>
      <xsl:attribute name="pos" select="@pos + 8"/>
      <xsl:element name="o">
        <xsl:attribute name="base" select="'Φ.dataized'"/>
        <xsl:element name="o">
          <xsl:for-each select="@*[name()!='const' and name()!='name']">
            <xsl:attribute name="{name()}">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:for-each>
          <xsl:if test="eo:abstract(.)">
            <xsl:attribute name="name">
              <xsl:value-of select="eo:unique-name($cname, ./parent::o, 0)"/>
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
