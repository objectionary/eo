<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="inline-cactoos" version="2.0">
  <!--
    Converts such EO code:
      [] > foo
        x > y
          $.a🌵2
        [] > a🌵2
          some > @

    to the next:
      [] > foo
        x > y
          [] >>
            some > @
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="auto" select="concat('a', $eo:cactoos)"/>
  <xsl:template match="o[contains(@base, concat('.', $auto))]" priority="0">
    <xsl:variable name="name" select="substring-after(@base, substring-before(@base, $auto))"/>
    <xsl:variable name="target" select="ancestor::o/o[@name=$name][1]"/>
    <xsl:element name="o">
      <xsl:if test="@as">
        <xsl:apply-templates select="@as"/>
      </xsl:if>
      <xsl:apply-templates select="$target/@*"/>
      <xsl:apply-templates select="$target/node()"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="o[starts-with(@name, $auto)]" priority="1">
    <!-- Nothing here -->
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
