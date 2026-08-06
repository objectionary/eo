<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="set-locators" version="2.0">
  <!--
  Here we go through all objects and add @loc attributes
  to all of them. The value of the attribute is a unique locator
  of the object.
  The locator function takes the object only and reaches the
  program through it, instead of taking the program as a second
  argument. Saxon 13.0 binds the arguments of a multi-argument
  function to the wrong values, once in a while, when one compiled
  stylesheet transforms many documents in parallel threads (which is
  what the "parse" goal does).
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_specials.xsl"/>
  <xsl:function name="eo:locator" as="xs:string">
    <xsl:param name="o" as="node()"/>
    <xsl:if test="name($o) != 'o'">
      <xsl:message terminate="yes">
        <xsl:text>Only 'o' XML elements are accepted here</xsl:text>
      </xsl:message>
    </xsl:if>
    <xsl:variable name="ret">
      <xsl:choose>
        <xsl:when test="$o/parent::o">
          <xsl:value-of select="eo:locator($o/parent::o)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$eo:program"/>
          <xsl:if test="root($o)/object/metas/meta[head='package']">
            <xsl:text>.</xsl:text>
            <xsl:value-of select="root($o)/object/metas/meta[head='package']/tail"/>
          </xsl:if>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>.</xsl:text>
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
            <xsl:when test="$o/@as">
              <xsl:value-of select="$o/@as"/>
            </xsl:when>
            <xsl:when test="starts-with($o/parent::o/@base, '.') and not($o/preceding-sibling::o)">
              <xsl:value-of select="$eo:rho"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$eo:alpha"/>
              <xsl:value-of select="count($o/preceding-sibling::o) - count($o/parent::o[starts-with(@base, '.')])"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:template match="o">
    <xsl:copy>
      <xsl:attribute name="loc" select="eo:locator(.)"/>
      <xsl:apply-templates select="node()|@* except @loc"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
