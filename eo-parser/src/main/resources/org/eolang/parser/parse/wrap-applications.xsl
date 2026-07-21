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

  We rewrite the @pipe node into an application whose @base refers to
  the preceding sibling by its @name. The preceding sibling (the
  formation, or a previous pipe) is left in place, so `| 5 > foo5`
  after `[x] > foo` is semantically identical to `foo 5 > foo5`.
  Chained pipes read the previous pipe's @name the same way, building
  left-associated applications.

  The @pipe marker is KEPT on the application (it used to be dropped
  here) so that the printer can round-trip the compact `| args > name`
  continuation line instead of expanding it into a two-line vertical
  application (#5684). It is a cosmetic hint only: downstream passes and
  the compiler read @base for semantics and ignore @pipe, exactly as
  they ignore the @local handle preserved for the same reason (#5681).

  When the pipe sits inside an argument block (its parent has a @base,
  so its siblings are collected as positional arguments), leaving the
  predecessor formation in place would give the enclosing application
  two arguments instead of one (#5526). We mark such a predecessor with
  @float-up; `vars-float-up` then hoists its definition to the nearest
  abstract object and drops the in-place argument slot, so only the pipe
  application reaches the enclosing application.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@pipe]">
    <xsl:copy>
      <xsl:attribute name="base">
        <xsl:value-of select="preceding-sibling::o[1]/@name"/>
      </xsl:attribute>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <!-- Predecessor formation of a pipe in an argument block: float it up. -->
  <xsl:template match="o[@name and not(@base) and ../@base and following-sibling::o[1][@pipe]]">
    <xsl:copy>
      <xsl:attribute name="float-up"/>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  <!-- Default copying -->
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
