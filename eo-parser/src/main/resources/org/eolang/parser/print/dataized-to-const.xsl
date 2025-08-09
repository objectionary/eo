<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="dataized-to-const" version="2.0">
  <!--
    Performs the reverse operation of "/org/eolang/parser/const-to-dataized.xsl"
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@base='.as-bytes' and o[position()=1 and @base='Φ.org.eolang.dataized']]">
    <xsl:variable name="argument" select="o[position()=1]/o[1]"/>
    <xsl:choose>
      <xsl:when test="exists($argument)">
        <xsl:element name="o">
          <xsl:apply-templates select="$argument/@*[name()!='as']"/>
          <xsl:attribute name="name" select="@name"/>
          <xsl:attribute name="const"/>
          <xsl:for-each select="$argument/o">
            <xsl:copy-of select="."/>
          </xsl:for-each>
          <xsl:if test="eo:has-data($argument)">
            <xsl:value-of select="$argument"/>
          </xsl:if>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
