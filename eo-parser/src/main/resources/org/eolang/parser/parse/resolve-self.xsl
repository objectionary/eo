<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs" id="resolve-self" version="2.0">
  <!--
  The "%" self-reference (§3.15) is syntactic sugar for the auto-name of
  the anonymous formation that surrounds it. The parser emits it as a
  base-less "<o self=''>" marker; here we simply substitute the name:
  set @base to the @name of the nearest enclosing anonymous formation
  (the closest ancestor whose auto-name carries the cactus prefix, §9.2)
  and drop @self. A "%" with no such ancestor is a compile-time error —
  there is no name to substitute.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@self]">
    <xsl:copy>
      <xsl:variable name="formation" select="ancestor::o[starts-with(@name, 'a🌵')][1]"/>
      <xsl:if test="exists($formation)">
        <xsl:attribute name="base" select="$formation/@name"/>
      </xsl:if>
      <xsl:apply-templates select="@* except @self"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="/object">
    <xsl:copy>
      <xsl:apply-templates select="(node() except errors)|@*"/>
      <xsl:variable name="errors" as="element()*">
        <xsl:for-each select="//o[@self and empty(ancestor::o[starts-with(@name, 'a🌵')])]">
          <xsl:element name="error">
            <xsl:attribute name="check" select="'resolve-self'"/>
            <xsl:attribute name="line" select="if (@line) then @line else 0"/>
            <xsl:attribute name="severity" select="'error'"/>
            <xsl:text>The % self-reference is only allowed inside an anonymous formation</xsl:text>
          </xsl:element>
        </xsl:for-each>
      </xsl:variable>
      <xsl:if test="not(empty($errors)) or exists(/object/errors)">
        <errors>
          <xsl:apply-templates select="/object/errors/error"/>
          <xsl:copy-of select="$errors"/>
        </errors>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
