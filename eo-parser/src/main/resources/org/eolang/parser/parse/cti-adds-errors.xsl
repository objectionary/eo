<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="https://www.eolang.org" id="cti-adds-errors" version="2.0" exclude-result-prefixes="eo">
  <!--
  For every cti objects add error messages.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object/errors">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:apply-templates select="//o[@base='Φ.cti']" mode="create"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="/object[not(errors)]">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:if test="//o[@base='Φ.cti']">
        <errors>
          <xsl:apply-templates select="//o[@base='Φ.cti']" mode="create"/>
        </errors>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
  <!-- A severity argument that is a string literal with one of the three legal values. -->
  <xsl:function name="eo:cti-has-valid-severity" as="xs:boolean">
    <xsl:param name="cti" as="element()"/>
    <xsl:sequence select="$cti/o[last() - 1]/@base = 'Φ.string' and eo:bytes-to-string($cti/o[last() - 1]/o[1]/o[1]/text()) = ('critical', 'error', 'warning')"/>
  </xsl:function>
  <xsl:template match="o[@base='Φ.cti' and count(o) &lt; 2]" mode="create">
    <error>
      <xsl:attribute name="check">
        <xsl:text>cti</xsl:text>
      </xsl:attribute>
      <xsl:attribute name="line">
        <xsl:value-of select="@line"/>
      </xsl:attribute>
      <xsl:attribute name="severity">
        <xsl:text>error</xsl:text>
      </xsl:attribute>
      <xsl:text>cti requires two arguments: a severity and a message</xsl:text>
    </error>
  </xsl:template>
  <xsl:template match="o[@base='Φ.cti' and count(o) &gt;= 2 and not(eo:cti-has-valid-severity(.))]" mode="create">
    <error>
      <xsl:attribute name="check">
        <xsl:text>cti</xsl:text>
      </xsl:attribute>
      <xsl:attribute name="line">
        <xsl:value-of select="@line"/>
      </xsl:attribute>
      <xsl:attribute name="severity">
        <xsl:text>error</xsl:text>
      </xsl:attribute>
      <xsl:text>cti severity must be a string literal, one of "critical", "error", "warning"</xsl:text>
    </error>
  </xsl:template>
  <xsl:template match="o[@base='Φ.cti' and count(o) &gt;= 2 and eo:cti-has-valid-severity(.)]" mode="create">
    <error>
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
    </error>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
