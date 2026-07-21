<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs" id="inline-cactoos" version="2.0">
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
  <!--
  A reference resolves to its own auto-name: given a base such as
  `ξ.ρ.a🌵4-2`, everything up to the cactus prefix is stripped, so the
  resolved name is the trailing `a🌵4-2`. This mirrors the `$name`
  computation in the inlining template below.
  -->
  <xsl:function name="eo:resolved-name" as="xs:string">
    <xsl:param name="base" as="xs:string"/>
    <xsl:sequence select="substring-after($base, substring-before($base, $auto))"/>
  </xsl:function>
  <!--
  Inline a reference to an auto-named abstract. A `? >> name` void
  (R-3.4.7 / R-3.10.12) also has a cactus name, so a reference that
  resolves to a void (or to nothing) is left untouched. A recursive
  helper (`? >> rec` whose body calls `rec` again) compiles to an
  auto-named abstract that references its own name; inlining it would
  copy that self-reference back in and fire this template forever, so
  such a target is also left untouched (both the reference and the
  abstraction stay in place).
  -->
  <xsl:template match="o[contains(@base, concat('.', $auto))]" priority="0">
    <xsl:variable name="name" select="eo:resolved-name(@base)"/>
    <xsl:variable name="target" select="ancestor::o/o[@name=$name][1]"/>
    <xsl:choose>
      <xsl:when test="exists($target) and not(eo:void($target)) and not(eo:recursive($target, $name))">
        <xsl:element name="o">
          <xsl:if test="@as">
            <xsl:apply-templates select="@as"/>
          </xsl:if>
          <xsl:apply-templates select="$target/@*"/>
          <xsl:apply-templates select="$target/node()"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="node()|@*"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!--
  Drop an inlined auto-named abstract; keep cactus-named voids and
  keep self-referential (recursive) abstracts, which are never inlined.
  -->
  <xsl:template match="o[starts-with(@name, $auto) and not(eo:void(.))]" priority="1">
    <xsl:if test="eo:recursive(., @name)">
      <xsl:copy>
        <xsl:apply-templates select="node()|@*"/>
      </xsl:copy>
    </xsl:if>
  </xsl:template>
  <!--
  Whether the auto-named abstract `$target` transitively references its
  own name `$name`, i.e. its subtree holds a reference that resolves
  back to `$name`. Such an abstract is recursive and must not be inlined.
  -->
  <xsl:function name="eo:recursive" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="exists($target//o[contains(@base, concat('.', $auto)) and eo:resolved-name(@base) = $name])"/>
  </xsl:function>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
