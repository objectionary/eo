<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="dataized-to-const" version="2.0">
  <!--
  Performs the reverse operation of "/org/eolang/parser/const-to-dataized.xsl":
  a `.as-bytes` over `Φ.dataized` wrapper is rebuilt as its dataized argument
  carrying `@const`.

  The wrapper's name is promoted onto the rebuilt node only when it is
  meaningful there: a real author name (`f > some!`) prints as the named const
  binding, and an abstract formation keeps its obfuscated cactus name so
  `to-eo-tree` renders the anonymous `[...] >>!` marker. A cactus name on a
  based value — a const file-local handle such as `a >> b!` folded onto its
  use site — is dropped, so the value lands as an anonymous const reference
  `a!` rather than a spurious `a >>!` line that keeps the obfuscated name and
  swallows the surrounding argument (#5810).
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@base='.as-bytes' and o[position()=1 and @base='Φ.dataized']]">
    <xsl:variable name="argument" select="o[position()=1]/o[1]"/>
    <xsl:choose>
      <xsl:when test="exists($argument)">
        <xsl:element name="o">
          <xsl:apply-templates select="$argument/@*[name()!='as']"/>
          <xsl:if test="@name and (eo:abstract($argument) or not(starts-with(@name, concat('a', $eo:cactoos))))">
            <xsl:attribute name="name" select="@name"/>
          </xsl:if>
          <xsl:attribute name="const"/>
          <xsl:for-each select="$argument/o">
            <xsl:apply-templates select="."/>
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
