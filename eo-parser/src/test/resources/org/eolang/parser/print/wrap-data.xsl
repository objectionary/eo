<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="wrap-data" version="2.0">
  <!--
  Converts this:

  <o base=".bytes">
    <o base=".eolang">
      <o base=".org">
        <o base="Q"/>
      </o>
    </o>
    00-00-00-00
  </o>

  to this:

  <o base="org.eolang.bytes">00-00-00-00</o>
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[eo:has-data(.)]">
    <xsl:apply-templates select="." mode="with-data"/>
  </xsl:template>
  <xsl:template match="o[@base='.bytes' and o[1][@base='.eolang' and o[1][@base='.org' and o[1][@base='Q']]]]" mode="with-data">
    <xsl:element name="o">
      <xsl:apply-templates select="@* except @base"/>
      <xsl:attribute name="base" select="'org.eolang.bytes'"/>
      <xsl:value-of select="text()"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
