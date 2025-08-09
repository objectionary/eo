<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="cti-adds-errors" version="2.0" exclude-result-prefixes="eo">
  <!--
  For every cti objects add error messages.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object/errors">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:apply-templates select="//o[@base='Φ.org.eolang.cti']" mode="create"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="/object[not(errors)]">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:if test="//o[@base='Φ.org.eolang.cti']">
        <errors>
          <xsl:apply-templates select="//o[@base='Φ.org.eolang.cti']" mode="create"/>
        </errors>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[@base='Φ.org.eolang.cti']" mode="create">
    <xsl:element name="error">
      <xsl:attribute name="check">
        <xsl:text>cti</xsl:text>
      </xsl:attribute>
      <xsl:attribute name="line">
        <xsl:value-of select="@line"/>
      </xsl:attribute>
      <xsl:attribute name="severity">
        <xsl:value-of select="eo:bytes-to-string(o[last() - 1]/o[1]/o[1]/text())"/>
      </xsl:attribute>
      <xsl:value-of select="eo:bytes-to-string(o[last()]/o[1]/o[1]/text())"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
