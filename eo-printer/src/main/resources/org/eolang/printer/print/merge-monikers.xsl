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

  For each formation, an eligible named bound attribute (see
  `eo:moniker-binding`) is merged onto the first bare `ξ.<name>` reference
  reachable through no intervening formation (so the name still binds to
  the same formation). A method-dispatch use such as `value.gte` or a
  reference that is itself a named attribute (such as `temp > @`) cannot
  host the binding and stays a reference. A reference carrying arguments
  (`r list`) can host it: the binding is inlined at the reference site and
  the arguments float onto it as a `| args` pipe continuation (§3.14).
  When no hostable reference exists, the binding is left in place.

  A binding with several hostable references still becomes a moniker,
  landing on the first one in document order (#5739). This deliberately
  gives up the print/parse fixpoint that a single-reference-only merge
  (#5707) guaranteed: canonical attribute ordering (#5706) can reorder the
  sibling bindings a reference sits inside, so "the first reference" may
  shift between passes and the printed moniker may move with it. Since only
  the compiler's obfuscated names are merged (#5738), this shift stays
  hidden inside auto-generated plumbing and never moves an author's name.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  The single attribute name a `ξ.<name>` reference resolves to, or the
  empty string for anything that is not a hostable reference. A named
  node (its `@name` would be clobbered by the binding), a trailing path,
  or a non-`ξ` base all disqualify it. Simple arguments do not: a
  reference carrying leaf arguments (`r list`) still resolves, and the
  merge floats them onto the inlined binding as a one-line `| args`
  continuation. An argument that carries its own arguments would force a
  multi-line `|` block, which is not a valid pipe continuation, so such a
  reference is left alone.
  -->
  <xsl:function name="eo:resolved-ref" as="xs:string">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="tail" select="substring-after($ref/@base, concat($eo:xi, '.'))"/>
    <xsl:sequence select="if (exists($ref/@base) and not(exists($ref/@name)) and starts-with($ref/@base, concat($eo:xi, '.')) and not(contains($tail, '.')) and not($ref/o[o])) then $tail else ''"/>
  </xsl:function>
  <!--
  Whether `$attr` is a formation attribute eligible to become a moniker:
  bound (an application or a formation, so not void), neither `φ`, a test,
  nor already floated with a pipe (`|`), and either auto-named with the
  cactus prefix (`a🌵`, §9.2) or carrying a file-local `@local` handle. The
  latter covers a recursive `>>`-named formation, whose obfuscated name
  `restore-local-names` promotes back to its handle (#5677); it is still
  compiler plumbing, not an author's declared member. Only such obfuscated
  names are merged; a real, author-chosen name such as `value` reads best
  on its own `... > name` line and stays standalone (#5738).
  -->
  <xsl:function name="eo:moniker-binding" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:sequence select="exists($attr/@name) and (starts-with($attr/@name, concat('a', $eo:cactoos)) or exists($attr/@local)) and not(exists($attr/@pipe)) and not(eo:void($attr)) and $attr/@name != $eo:phi and not(eo:test-attr($attr)) and eo:abstract($attr/..)"/>
  </xsl:function>
  <!--
  The bare `ξ.<name>` references, in document order, that can host the
  binding `$attr`: a bare reference whose nearest formation ancestor is the
  binding's own owner and that does not sit inside the binding itself.
  -->
  <xsl:function name="eo:moniker-refs" as="element()*">
    <xsl:param name="attr" as="element()"/>
    <xsl:variable name="owner" select="$attr/.."/>
    <xsl:sequence select="$owner//o[eo:resolved-ref(.) = $attr/@name and (ancestor::o[eo:abstract(.)][1] is $owner) and not(ancestor::o[. is $attr])]"/>
  </xsl:function>
  <!--
  The binding that a reference `$ref` should be replaced with, or the empty
  sequence when `$ref` hosts no binding (not a bare reference, no eligible
  binding, or not the first hosting reference).
  -->
  <xsl:function name="eo:hosted-binding" as="element()*">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="owner" select="$ref/ancestor::o[eo:abstract(.)][1]"/>
    <xsl:variable name="binding" select="$owner/o[@name = eo:resolved-ref($ref) and eo:moniker-binding(.)][1]"/>
    <xsl:sequence select="if (exists($binding) and (eo:moniker-refs($binding)[1] is $ref)) then $binding else ()"/>
  </xsl:function>
  <!--
  Replace the first hosting reference with the merged binding: keep the
  reference's positional `@as`, take the binding's other attributes and its
  children. When the reference carried arguments (`r list`), the inlined
  binding cannot hold them, so they float onto it as a following `| args`
  pipe continuation (§3.14) whose base names the just-inlined binding.
  -->
  <xsl:template match="o[exists(eo:hosted-binding(.))]" priority="1">
    <xsl:variable name="binding" select="eo:hosted-binding(.)"/>
    <xsl:element name="o">
      <xsl:apply-templates select="@as"/>
      <xsl:apply-templates select="$binding/@*[name() != 'as']"/>
      <xsl:apply-templates select="$binding/node()"/>
    </xsl:element>
    <xsl:if test="o">
      <xsl:element name="o">
        <xsl:attribute name="pipe"/>
        <xsl:attribute name="base" select="concat($eo:xi, '.', $binding/@name)"/>
        <xsl:apply-templates select="o"/>
      </xsl:element>
    </xsl:if>
  </xsl:template>
  <!--
  Drop the standalone binding once it has been merged onto a reference.
  -->
  <xsl:template match="o[eo:moniker-binding(.) and exists(eo:moniker-refs(.))]" priority="1"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
