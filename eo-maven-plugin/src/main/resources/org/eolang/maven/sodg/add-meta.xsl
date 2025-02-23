<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="add-meta" version="2.0">
  <!--
  Here we add a vertex with meta information and attach it to v0
  by the attributes that start with "+".
  -->
  <xsl:import href="/org/eolang/maven/sodg/_macros.xsl"/>
  <xsl:param name="name"/>
  <xsl:param name="value"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/sodg">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:variable name="v">
        <xsl:text>meta-</xsl:text>
        <xsl:value-of select="$name"/>
      </xsl:variable>
      <xsl:call-template name="i">
        <xsl:with-param name="name" select="'ADD'"/>
        <xsl:with-param name="args" as="item()*">
          <xsl:sequence>
            <xsl:value-of select="eo:var($v)"/>
          </xsl:sequence>
        </xsl:with-param>
        <xsl:with-param name="comment">
          <xsl:text>This is a meta</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="i">
        <xsl:with-param name="name" select="'BIND'"/>
        <xsl:with-param name="args" as="item()*">
          <xsl:sequence>
            <xsl:text>ν0</xsl:text>
          </xsl:sequence>
          <xsl:sequence>
            <xsl:value-of select="eo:var($v)"/>
          </xsl:sequence>
          <xsl:sequence>
            <xsl:value-of select="concat('+', $name)"/>
          </xsl:sequence>
        </xsl:with-param>
        <xsl:with-param name="comment">
          <xsl:text>This is a meta</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="i">
        <xsl:with-param name="name" select="'PUT'"/>
        <xsl:with-param name="args" as="item()*">
          <xsl:sequence>
            <xsl:value-of select="eo:var($v)"/>
          </xsl:sequence>
          <xsl:sequence>
            <xsl:value-of select="replace($value, ' ', '-')"/>
          </xsl:sequence>
        </xsl:with-param>
        <xsl:with-param name="comment">
          <xsl:text>This is a meta</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:copy>
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
