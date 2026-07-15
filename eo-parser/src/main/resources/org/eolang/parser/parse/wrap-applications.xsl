<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="wrap-applications" version="2.0">
  <!--
  A pipe-application line (§3.14) emits a base-less marker node whose
  head is the same-indent object above it:

  <o name="foo"/>              (the formation, named)
  <o pipe="" name="foo5">      (| 5 > foo5)
    <o base="Φ.number">5</o>
  </o>

  We rewrite the @pipe node into an ordinary application whose @base
  refers to the preceding sibling by its @name, and drop @pipe. The
  preceding sibling (the formation, or a previous pipe) is left in
  place, so `| 5 > foo5` after `[x] > foo` is identical in XMIR to
  `foo 5 > foo5`. Chained pipes read the previous pipe's @name the same
  way, building left-associated applications.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@pipe]">
    <xsl:copy>
      <xsl:attribute name="base">
        <xsl:value-of select="preceding-sibling::o[1]/@name"/>
      </xsl:attribute>
      <xsl:apply-templates select="@* except @pipe"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <!-- Default copying -->
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
