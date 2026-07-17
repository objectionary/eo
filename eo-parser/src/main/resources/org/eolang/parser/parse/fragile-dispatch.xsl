<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="fragile-dispatch" version="2.0" exclude-result-prefixes="eo">
  <!--
  Warn when a regular `.` dispatch is performed directly on a fragile
  `?.` dispatch that was not applied (R-3.5.3b). The offending link is a
  method dispatch (@method) without @fragile whose immediately-preceding
  sibling is a childless fragile link (a bare `?.` with no application
  args). An application carries arg children, so it does not match.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="eo:flawed" select="//o[@method and not(@fragile) and preceding-sibling::o[1][@fragile and not(o)]]"/>
  <xsl:template match="/object/errors">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:apply-templates select="$eo:flawed" mode="warn"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="/object[not(errors)]">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:if test="$eo:flawed">
        <errors>
          <xsl:apply-templates select="$eo:flawed" mode="warn"/>
        </errors>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o" mode="warn">
    <xsl:element name="error">
      <xsl:attribute name="check">
        <xsl:text>fragile-dispatch</xsl:text>
      </xsl:attribute>
      <xsl:attribute name="severity">
        <xsl:text>warning</xsl:text>
      </xsl:attribute>
      <xsl:attribute name="line">
        <xsl:value-of select="@line"/>
      </xsl:attribute>
      <xsl:attribute name="pos">
        <xsl:value-of select="@pos"/>
      </xsl:attribute>
      <xsl:text>regular dispatch on a fragile object requires `?.`</xsl:text>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
