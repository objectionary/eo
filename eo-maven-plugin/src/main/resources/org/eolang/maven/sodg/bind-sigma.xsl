<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="bind-sigma" version="2.0">
  <!--
  Here we BIND all object formations to their parents using \rho and \sigma.
  We don't bind "data" and "lambda" objects, through.
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
  <xsl:template match="o[not(@level) and eo:abstract(.)]" mode="sodg" priority="1">
    <xsl:variable name="o" select="."/>
    <xsl:if test="not(@loc)">
      <xsl:message terminate="yes">
        <xsl:text>The object doesn't have @loc, how come?</xsl:text>
      </xsl:message>
    </xsl:if>
    <xsl:call-template name="i">
      <xsl:with-param name="name" select="'BIND'"/>
      <xsl:with-param name="args" as="item()*">
        <xsl:sequence>
          <xsl:value-of select="eo:var(@loc)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:choose>
            <xsl:when test="@parent">
              <xsl:value-of select="eo:var(//o[@name=$o/@parent]/@loc)"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="eo:var(ancestor::*[eo:abstract(.) or name()='objects'][1]/@loc)"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:text>σ</xsl:text>
        </xsl:sequence>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="o" mode="sodg">
    <!-- ignore it -->
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*" mode="#current"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
