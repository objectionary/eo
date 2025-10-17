<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="resolve-compacts" version="2.0">
  <!--
    Here we go through all `@compact:oname` bases and resolve it's FQN:
    1. If `oname` is defined in the current scope => `oname`
    2. Else if an alias exists for `oname` => use FQN from alias
    3. Otherwise => `Φ.org.eolang.oname`
  -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="@base[starts-with(., '@compact:')]">
    <xsl:variable name="q" select="substring-after(., '@compact:')"/>
    <xsl:variable name="head" select="substring-before(concat($q, '.'), '.')"/>
    <xsl:variable name="alias" select="/object/metas/meta[head='alias' and part[1]=$head]/part[last()]"/>
    <xsl:choose>
      <xsl:when test="parent::o/parent::o/o[@name = $head]">
        <xsl:attribute name="base">
          <xsl:value-of select="$q"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:when test="string-length($alias) &gt; 0">
        <xsl:attribute name="base">
          <xsl:value-of select="$alias"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
        <xsl:attribute name="base">
          <xsl:value-of select="concat('Φ.org.eolang.', $q)"/>
        </xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
