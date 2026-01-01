<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="set-original-names" version="2.0">
  <!--
  Here we go through all objects and add @original-name attributes
  to all of them. The value of the attribute is a FQN
  of the object.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_specials.xsl"/>
  <xsl:function name="eo:original-name" as="xs:string">
    <xsl:param name="program" as="node()"/>
    <xsl:param name="o" as="node()"/>
    <xsl:if test="name($o) != 'o'">
      <xsl:message terminate="yes">
        <xsl:text>Only 'o' XML elements are accepted here</xsl:text>
      </xsl:message>
    </xsl:if>
    <xsl:variable name="parent" select="$o/parent::o"/>
    <xsl:variable name="ret">
      <xsl:if test="$parent">
        <xsl:value-of select="eo:original-name($program, $parent)"/>
        <xsl:text>.</xsl:text>
      </xsl:if>
      <xsl:choose>
        <xsl:when test="$o/@name">
          <xsl:choose>
            <xsl:when test="$o/@name = '@'">
              <xsl:value-of select="$eo:phi"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$o/@name"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise>
          <xsl:choose>
            <xsl:when test="starts-with($parent/@base, '.') and not($o/preceding-sibling::o)">
              <xsl:value-of select="$eo:rho"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$eo:alpha"/>
              <xsl:value-of select="count($o/preceding-sibling::o) - count($parent[starts-with(@base, '.')])"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:template match="o[not(@original-name)]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:attribute name="original-name" select="eo:original-name(/object, .)"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
