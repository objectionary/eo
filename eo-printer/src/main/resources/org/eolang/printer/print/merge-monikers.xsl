<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs" id="merge-monikers" version="2.0">
  <!--
  Merges a standalone named binding back onto its bare-reference use site,
  restoring the shorter "moniker" spelling. The parser hoists a `> name`
  binding up to its enclosing formation regardless of where it is written,
  so the moniker spelling (the binding written in place of its bare
  reference, such as `number num.as-bytes > value` sitting where a bare
  `value` would appear) and the expanded spelling the printer would
  otherwise emit (a bare `value` reference plus a separate `... > value`
  binding line) parse to one and the same object graph. This pass turns
  the expanded spelling back into the moniker.

  For each formation, a named bound attribute that is neither void, `φ`,
  a test, nor pipe-floated is merged onto its bare `ξ.<name>` reference
  reachable through no intervening formation (so the name still binds to
  the same formation). A method-dispatch use such as `value.gte`, a
  reference carrying arguments, or a reference that is itself a named
  attribute (such as `temp > @`) cannot host the binding and stays a
  reference.

  Only a binding with exactly one hostable reference is merged. With two
  or more, the choice of a host would not survive the print/parse round
  trip: canonical attribute ordering (#5706) reorders the sibling bindings
  a reference sits inside, so "the first reference" flips between passes
  and the printer stops converging. Such a binding, and one with no bare
  reference at all, is left in place.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  The single attribute name a bare `ξ.<name>` reference resolves to, or the
  empty string for anything that is not a hostable bare reference. A named
  node (its `@name` would be clobbered by the binding), a trailing path,
  arguments, or a non-`ξ` base all disqualify it.
  -->
  <xsl:function name="eo:resolved-ref" as="xs:string">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="tail" select="substring-after($ref/@base, concat($eo:xi, '.'))"/>
    <xsl:sequence select="if (exists($ref/@base) and not(exists($ref/@name)) and starts-with($ref/@base, concat($eo:xi, '.')) and not(contains($tail, '.')) and not($ref/o)) then $tail else ''"/>
  </xsl:function>
  <!--
  Whether `$attr` is a formation attribute eligible to become a moniker:
  named, bound (has a base), and neither void, `φ`, a test, nor already
  floated with a pipe (`|`).
  -->
  <xsl:function name="eo:moniker-binding" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:sequence select="exists($attr/@name) and exists($attr/@base) and not(exists($attr/@pipe)) and not(eo:void($attr)) and $attr/@name != $eo:phi and not(eo:test-attr($attr)) and eo:abstract($attr/..)"/>
  </xsl:function>
  <!--
  The bare `ξ.<name>` references that can host the binding `$attr`: a bare
  reference whose nearest formation ancestor is the binding's own owner and
  that does not sit inside the binding itself.
  -->
  <xsl:function name="eo:moniker-refs" as="element()*">
    <xsl:param name="attr" as="element()"/>
    <xsl:variable name="owner" select="$attr/.."/>
    <xsl:sequence select="$owner//o[eo:resolved-ref(.) = $attr/@name and (ancestor::o[eo:abstract(.)][1] is $owner) and not(ancestor::o[. is $attr])]"/>
  </xsl:function>
  <!--
  The binding that a reference `$ref` should be replaced with, or the empty
  sequence when `$ref` hosts no binding (not a bare reference, no eligible
  binding, or a binding with anything other than this single reference).
  -->
  <xsl:function name="eo:hosted-binding" as="element()*">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="owner" select="$ref/ancestor::o[eo:abstract(.)][1]"/>
    <xsl:variable name="binding" select="$owner/o[@name = eo:resolved-ref($ref) and eo:moniker-binding(.)][1]"/>
    <xsl:sequence select="if (exists($binding) and (count(eo:moniker-refs($binding)) = 1) and (eo:moniker-refs($binding)[1] is $ref)) then $binding else ()"/>
  </xsl:function>
  <!--
  Replace the first hosting reference with the merged binding: keep the
  reference's positional `@as`, take the binding's other attributes and its
  children.
  -->
  <xsl:template match="o[exists(eo:hosted-binding(.))]" priority="1">
    <xsl:variable name="binding" select="eo:hosted-binding(.)"/>
    <xsl:element name="o">
      <xsl:apply-templates select="@as"/>
      <xsl:apply-templates select="$binding/@*[name() != 'as']"/>
      <xsl:apply-templates select="$binding/node()"/>
    </xsl:element>
  </xsl:template>
  <!--
  Drop the standalone binding once it has been merged onto a reference.
  -->
  <xsl:template match="o[eo:moniker-binding(.) and count(eo:moniker-refs(.)) = 1]" priority="1"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
