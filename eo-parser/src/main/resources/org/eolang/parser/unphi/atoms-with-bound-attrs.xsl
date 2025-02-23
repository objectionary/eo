<?xml version="1.0"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:eo="https://www.eolang.org" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="atoms-with-bound-attrs" version="2.0">
  <!--
  Convert such expression in XMIR:
    x ↦ ⟦
      text ↦ Φ.org.eolang.bytes (α0 ↦ ⟦ Δ ⤍ 48-65-6C-6C-6F-20-77-6F-72-6C-64-0A ⟧),
      size ↦ Φ.org.eolang.bytes (α0 ↦ ⟦ Δ ⤍ 00-00-00-00-00-00-00-08 ⟧),
      λ ⤍ Lorg_eolang_io_stdout
    ⟧
  to this:
    x ↦ Φ.org.eolang.io.stdout(
      text ↦ Φ.org.eolang.bytes (α0 ↦ ⟦ Δ ⤍ 48-65-6C-6C-6F-20-77-6F-72-6C-64-0A ⟧),
      size ↦ Φ.org.eolang.bytes (α0 ↦ ⟦ Δ ⤍ 00-00-00-00-00-00-00-08 ⟧)
    )
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@atom and starts-with(@atom, 'L')]">
    <xsl:apply-templates select="." mode="atom"/>
  </xsl:template>
  <xsl:template match="o[not(o[@base='∅']) and count(o[@base!='∅'])&gt;0]" mode="atom">
    <xsl:element name="o">
      <xsl:attribute name="name" select="@name"/>
      <xsl:attribute name="base">
        <xsl:text>Q.</xsl:text>
        <xsl:for-each select="tokenize(@atom,'_')">
          <xsl:choose>
            <xsl:when test="position()=1">
              <xsl:value-of select="substring(., 2)"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>.</xsl:text>
              <xsl:choose>
                <xsl:when test=".='φ'">
                  <xsl:text>@</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="."/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:attribute>
      <xsl:for-each select="o">
        <xsl:element name="o">
          <xsl:apply-templates select="@* except @name"/>
          <xsl:attribute name="as" select="@name"/>
          <xsl:copy-of select="*"/>
        </xsl:element>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
