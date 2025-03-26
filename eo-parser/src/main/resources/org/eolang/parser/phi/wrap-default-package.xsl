<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:eo="https://www.eolang.org" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="wrap-default-package" version="2.0">
  <!--
    Here we change elements:

    <o base=".eolang">
      <o base=".org">
        <o base="Q"/>
      </o>
    </o>

    with:

    <o base="org.eolang"/>
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[starts-with(@base, '.') and o[1][@base='.eolang' and o[1][@base='.org' and o[1][@base='Q']]]]">
    <xsl:element name="o">
      <xsl:apply-templates select="@* except @base"/>
      <xsl:attribute name="base">
        <xsl:text>org.eolang</xsl:text>
        <xsl:value-of select="@base"/>
      </xsl:attribute>
      <xsl:for-each select="o[position()!=1]">
        <xsl:apply-templates select="."/>
      </xsl:for-each>
      <xsl:if test="eo:has-data(.)">
        <xsl:value-of select="eo:read-data(.)"/>
      </xsl:if>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
