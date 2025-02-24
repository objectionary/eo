<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="epsilon-bindings" version="2.0">
  <!--
  Here we add xi-edges to objects that ARE other objects.
  -->
  <xsl:import href="/org/eolang/maven/sodg/_macros.xsl"/>
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/sodg">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:apply-templates select="/program/objects//o" mode="sodg"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[@base and not(@level) and not(starts-with(@base, '.')) and not(o) and not(eo:has-data(.))]" mode="sodg" priority="1">
    <xsl:call-template name="i">
      <xsl:with-param name="name" select="'BIND'"/>
      <xsl:with-param name="args" as="item()*">
        <xsl:sequence>
          <xsl:value-of select="eo:var(@loc)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:value-of select="eo:var(eo:base-to-loc(.))"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:choose>
            <xsl:when test="@base = '$'">
              <xsl:text>ξ</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>ε</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:sequence>
      </xsl:with-param>
      <xsl:with-param name="comment">
        <xsl:text>This is a hard reference</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="o" mode="sodg">
    <!-- ignore them -->
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*" mode="#current"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
