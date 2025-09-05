<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="expand-qqs" version="2.0">
  <!--
  Replace 'Φ̇' to 'org.eolang' for:
  - @base attribute for all objects;
  - child elements' text of all metas
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@base='Φ̇']">
    <xsl:copy>
      <xsl:attribute name="base">.eolang</xsl:attribute>
      <xsl:copy-of select="@*[name()!='base']"/>
      <xsl:element name="o">
        <xsl:attribute name="base">.org</xsl:attribute>
        <xsl:copy-of select="@*[name()!='base']"/>
        <xsl:element name="o">
          <xsl:attribute name="base">Φ</xsl:attribute>
          <xsl:copy-of select="@*[name()!='base']"/>
        </xsl:element>
      </xsl:element>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="meta/*[text()[matches(., '^Φ̇\..*')]]">
    <xsl:copy>
      <xsl:value-of select="replace(./text(), '^Φ̇\.', 'Φ.org.eolang.')"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
