<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="wrap-applications" version="2.0">
  <!--
  A pipe-application line (§3.14) emits a base-less marker node whose
  head is the same-indent object above it. There are two cases.

  1. Named predecessor — refer to it by name and drop @pipe, leaving the
  predecessor in place:

  <o name="foo"/>              (the formation, named)
  <o pipe="" name="foo5">      (| 5 > foo5)
    <o base="Φ.number">5</o>
  </o>
    =>
  <o name="foo"/>
  <o base="foo" name="foo5">
    <o base="Φ.number">5</o>
  </o>

  So `| 5 > foo5` after `[x] > foo` is identical in XMIR to
  `foo 5 > foo5`. Chained pipes read the previous pipe's @name the same
  way, building left-associated applications.

  2. Nameless-formation predecessor — there is no name to refer to, so we
  fuse: the pipe's @name (and @const) and its argument children move into
  the formation node, and the pipe node is dropped. This yields the
  phi-calculus fusion, the same XMIR a `(body > [x]) 5` paren group
  produces:

  <o>                          (nameless formation)
    <o base="∅" name="x"/>
    <o base="ξ.x.plus" name="φ">…</o>
  <o pipe="" name="y">         (| 5 > y)
    <o base="Φ.number">5</o>
  </o>
    =>
  <o name="y">
    <o base="∅" name="x"/>
    <o base="ξ.x.plus" name="φ">…</o>
    <o base="Φ.number">5</o>
  </o>
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- Pipe on a named predecessor: application by name. -->
  <xsl:template match="o[@pipe][preceding-sibling::o[1]/@name]">
    <xsl:copy>
      <xsl:attribute name="base">
        <xsl:value-of select="preceding-sibling::o[1]/@name"/>
      </xsl:attribute>
      <xsl:apply-templates select="@* except @pipe"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <!-- Pipe on a nameless formation: absorbed into it, so the node is dropped. -->
  <xsl:template match="o[@pipe][not(preceding-sibling::o[1]/@name)]"/>
  <!-- Nameless formation directly followed by a pipe: fuse the pipe in. -->
  <xsl:template match="o[not(@name) and not(@base) and following-sibling::o[1][@pipe]]">
    <xsl:variable name="pipe" select="following-sibling::o[1]"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:copy-of select="$pipe/@name"/>
      <xsl:copy-of select="$pipe/@const"/>
      <xsl:apply-templates select="node()"/>
      <xsl:apply-templates select="$pipe/node()"/>
    </xsl:copy>
  </xsl:template>
  <!-- Default copying -->
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
