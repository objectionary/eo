<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="add-default-package" version="2.0">
  <!--
  Here we go through all objects that are not:
    1. methods (starts with .)
    2. @, Q, ^ or $
    3. mentioned in aliases

  and add default package to them.

  We ignore objects that are present in aliases with their exact
  names. For example, this object 'hello' won't be touched, we
  won't think that it belongs to org.eolang package:

  +alias hello

  # No comment.
  [] > app
    hello > @
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@base]">
    <xsl:apply-templates select="." mode="with-base"/>
  </xsl:template>
  <xsl:template match="/object/metas/meta[head='also' or head='decorate']/(tail|part)">
    <xsl:apply-templates select="." mode="meta"/>
  </xsl:template>
  <xsl:template match="*[not(contains(text(), ' '))]" mode="meta">
    <xsl:copy>
      <xsl:choose>
        <xsl:when test="starts-with(text(), 'org.eolang')">
          <xsl:text>Φ.</xsl:text>
          <xsl:value-of select="text()"/>
        </xsl:when>
        <xsl:when test="not(starts-with(text(), 'Φ.org.eolang')) and not(starts-with(text(), 'Φ̇.org.eolang'))">
          <xsl:value-of select="'Φ.org.eolang.'"/>
          <xsl:value-of select="text()"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="text()"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[starts-with(@base, 'org.eolang')]" mode="with-base">
    <xsl:copy>
      <xsl:attribute name="base">
        <xsl:text>Φ.</xsl:text>
        <xsl:value-of select="@base"/>
      </xsl:attribute>
      <xsl:apply-templates select="node()|@* except @base"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[not(contains(@base, '.'))]" mode="with-base">
    <xsl:apply-templates select="." mode="no-dots"/>
  </xsl:template>
  <xsl:template match="o[@base!='φ' and @base!='Φ' and @base!='Φ̇' and @base!='ρ' and @base!='∅' and @base!='ξ']" mode="no-dots">
    <xsl:apply-templates select="." mode="no-specials"/>
  </xsl:template>
  <xsl:template match="o[not(@base=/object/metas/meta[head='alias']/part[1])]" mode="no-specials">
    <xsl:copy>
      <xsl:attribute name="base">
        <xsl:text>Φ.org.eolang.</xsl:text>
        <xsl:value-of select="@base"/>
      </xsl:attribute>
      <xsl:apply-templates select="node()|@* except @base"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
