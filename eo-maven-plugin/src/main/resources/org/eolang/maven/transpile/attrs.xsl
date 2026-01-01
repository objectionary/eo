<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="attrs" version="2.0">
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="class[not(@base)]/o[@name]">
    <xsl:apply-templates select="." mode="with-attributes"/>
  </xsl:template>
  <xsl:template match="o/o[@name]" mode="abstracts">
    <xsl:apply-templates select="." mode="with-attributes"/>
  </xsl:template>
  <xsl:template match="nested/o">
    <xsl:apply-templates select="." mode="with-attributes"/>
  </xsl:template>
  <xsl:template match="*" mode="with-attributes">
    <xsl:element name="attr">
      <xsl:apply-templates select="@name"/>
      <xsl:variable name="type">
        <xsl:choose>
          <xsl:when test="@base and @base!=$eo:empty">
            <xsl:text>bound</xsl:text>
          </xsl:when>
          <xsl:when test="@base and @base=$eo:empty">
            <xsl:text>void</xsl:text>
          </xsl:when>
          <xsl:when test="eo:atom(.)">
            <xsl:text>atom</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>abstract</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="inner">
        <xsl:element name="{$type}">
          <xsl:if test="$type='abstract'">
            <xsl:apply-templates select="@*"/>
            <xsl:apply-templates select="node()" mode="abstracts"/>
          </xsl:if>
          <xsl:if test="$type!='abstract'">
            <xsl:copy>
              <xsl:apply-templates select="node()|@*"/>
            </xsl:copy>
          </xsl:if>
        </xsl:element>
      </xsl:variable>
      <xsl:copy-of select="$inner"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
