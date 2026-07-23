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

  For each formation, an eligible bound attribute (see `eo:moniker-binding`)
  is merged onto the first hostable reference reachable through no intervening
  formation (so the name still binds to the same formation). Two spellings
  host it: a bare `ξ.<name>` reference and a single trailing dispatch
  `ξ.<name>.<seg>` (#5782). A bare reference must be unnamed, but a dispatch
  may host onto a named reference (such as `q. > @`), since the reversed
  dispatch keeps the reference's own `@name` (#5794). A reference carrying
  arguments (`r list`) can host it: for a bare reference the leaf arguments
  float onto it as a `| args` pipe continuation (§3.14), for a dispatch they
  become the reversed dispatch's method args. When no hostable reference
  exists, the binding is left in place.

  A binding with several references still becomes a moniker when its host is
  a bare reference: the inline keeps the binding's name in place, so the
  other references resolve to it (#5739). A binding reachable only through
  dispatches (#5782) hosts on one of them, whose receiver prints
  anonymously, so it is merged only when referenced exactly once (see
  `eo:merges`); a `&gt;&gt;` handle dispatched through several members, such
  as clock's `timeb` used by both `timeb.time.times` and `timeb.millitm`, is
  left standalone rather than stranding the un-hosted dispatches.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  The single attribute name a hostable `ξ.<name>` reference resolves to, or
  the empty string for anything that is not hostable. Two shapes host a
  binding: a bare reference `ξ.<name>` and a single trailing dispatch segment
  `ξ.<name>.<seg>`, a reversed dispatch spelled forward whose receiver
  `<name>` can be inlined exactly as the bare case is, only as a reversed
  dispatch's receiver rather than a plain one (#5782). Both resolve to
  `<name>`. A bare reference may carry leaf arguments (`r list`), which the
  merge floats onto the inlined binding as a one-line `| args` continuation
  (#5678); an argument that carries its own arguments would force a
  multi-line `|` block and disqualifies it. A multi-segment path or a non-`ξ`
  base disqualify it, as does a named node for the bare case only — a bare
  inline would clobber the reference's `@name`, but a dispatch keeps it (#5794).
  -->
  <xsl:function name="eo:resolved-ref" as="xs:string">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="tail" select="substring-after($ref/@base, concat($eo:xi, '.'))"/>
    <xsl:sequence select="if (exists($ref/@base) and not(exists($ref/@name)) and starts-with($ref/@base, concat($eo:xi, '.')) and not(contains($tail, '.')) and not($ref/o[o])) then $tail else if (eo:dispatch-seg($ref) != '') then substring-before($tail, '.') else ''"/>
  </xsl:function>
  <!--
  The trailing dispatch segment of a hostable single-segment dispatch
  reference `ξ.<name>.<seg>` — the receiver name `<name>` carries no further
  dot, so exactly one dispatch segment `<seg>` trails it — or the empty string
  for a bare reference, a non-`ξ` base, or a multi-segment chain such as
  `ξ.a.b.c` (which would need deeply nested reversed dispatches and is
  deliberately left expanded, #5782). Unlike a bare reference, a dispatch may
  carry arguments and may be a named node: it inlines to a reversed dispatch
  that keeps both (#5794).
  -->
  <xsl:function name="eo:dispatch-seg" as="xs:string">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="tail" select="substring-after($ref/@base, concat($eo:xi, '.'))"/>
    <xsl:sequence select="if (exists($ref/@base) and starts-with($ref/@base, concat($eo:xi, '.')) and contains($tail, '.') and substring-before($tail, '.') != '' and not(contains(substring-after($tail, '.'), '.'))) then substring-after($tail, '.') else ''"/>
  </xsl:function>
  <!--
  Whether `$attr` is a formation attribute eligible to become a moniker:
  either auto-named with the cactus prefix (`a🌵`, §9.2) or carrying a
  file-local `@local` handle (a recursive `>>`-named formation whose obfuscated
  name `restore-local-names` promotes back to its handle, #5677), either bound
  (has a base) or an abstract formation (#5794), and neither void, `φ`, a test,
  nor already floated with a pipe (`|`). Only the compiler's obfuscated names
  are merged; a real, author-chosen name such as `value` reads best on its own
  `... > name` line and stays standalone (#5738).
  -->
  <xsl:function name="eo:moniker-binding" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:sequence select="exists($attr/@name) and (starts-with($attr/@name, concat('a', $eo:cactoos)) or exists($attr/@local)) and (exists($attr/@base) or eo:abstract($attr)) and not(exists($attr/@pipe)) and not(eo:void($attr)) and $attr/@name != $eo:phi and not(eo:test-attr($attr)) and eo:abstract($attr/..)"/>
  </xsl:function>
  <!--
  The references, in document order, that can host the binding `$attr`: a bare
  `ξ.<name>` reference or a single-segment dispatch `ξ.<name>.<seg>` (#5782)
  whose nearest formation ancestor is the binding's own owner and that does
  not sit inside the binding itself. Bare references are listed first, so a
  binding with both spellings still folds into the shorter bare inline and a
  dispatch hosts only when no bare reference is available.
  -->
  <xsl:function name="eo:moniker-refs" as="element()*">
    <xsl:param name="attr" as="element()"/>
    <xsl:variable name="owner" select="$attr/.."/>
    <xsl:variable name="refs" select="$owner//o[eo:resolved-ref(.) = $attr/@name and (ancestor::o[eo:abstract(.)][1] is $owner) and not(ancestor::o[. is $attr])]"/>
    <xsl:sequence select="($refs[eo:dispatch-seg(.) = ''], $refs[eo:dispatch-seg(.) != ''])"/>
  </xsl:function>
  <!--
  Whether the binding `$attr` is referenced exactly once, counting every
  reference to its name outside its own subtree. Used to guard the dispatch
  host below, which prints its receiver anonymously and so cannot serve more
  than one reference. Mirrors `restore-local-names`' `eo:reducible`.
  -->
  <xsl:function name="eo:sole-ref" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:sequence select="count($attr/..//o[$attr/@name = tokenize(@base, '\.') and not(ancestor::o[. is $attr])]) = 1"/>
  </xsl:function>
  <!--
  Whether the binding `$attr` becomes a moniker, hosted on its first hostable
  reference. A bare host inlines the binding in place keeping its name, so
  every other reference still resolves to it and the binding may be referenced
  more than once (#5739). A dispatch host (no bare reference exists, #5782)
  inlines the binding as a reversed dispatch's receiver, which prints
  anonymously; it therefore hosts only a binding referenced exactly once, or
  the other references — such as clock's `timeb.time.times` alongside the
  hosted `timeb.millitm` — would be stranded with no binding.
  -->
  <xsl:function name="eo:merges" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:variable name="refs" select="eo:moniker-refs($attr)"/>
    <xsl:sequence select="exists($refs) and (exists($refs[eo:dispatch-seg(.) = '']) or eo:sole-ref($attr))"/>
  </xsl:function>
  <!--
  The binding that a reference `$ref` should be replaced with, or the empty
  sequence when `$ref` hosts no binding (not a hostable reference, no eligible
  binding, a binding a dispatch host cannot absorb, or not the first hosting
  reference).
  -->
  <xsl:function name="eo:hosted-binding" as="element()*">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="owner" select="$ref/ancestor::o[eo:abstract(.)][1]"/>
    <xsl:variable name="binding" select="$owner/o[@name = eo:resolved-ref($ref) and eo:moniker-binding(.)][1]"/>
    <xsl:sequence select="if (exists($binding) and eo:merges($binding) and (eo:moniker-refs($binding)[1] is $ref)) then $binding else ()"/>
  </xsl:function>
  <!--
  Replace the first hosting reference with the merged binding, always keeping
  the reference's positional `@as`. A bare reference becomes the binding
  inlined in place (its other attributes and its children); when it carried
  leaf arguments (`r list`) they float onto the inline as a following `| args`
  pipe continuation (§3.14) whose base names the just-inlined binding (#5678).
  A single-segment dispatch `ξ.<name>.<seg>` becomes a reversed dispatch
  `<seg>.` whose receiver is that inlined binding and whose arguments are the
  reference's own children — the equivalent inline for a dispatch use (#5782).
  The dispatch also keeps the reference's own `@name`, so a named use such as
  `q. > @` over an anonymous formation round-trips (#5794).
  -->
  <xsl:template match="o[exists(eo:hosted-binding(.))]" priority="1">
    <xsl:variable name="binding" select="eo:hosted-binding(.)"/>
    <xsl:variable name="seg" select="eo:dispatch-seg(.)"/>
    <xsl:choose>
      <xsl:when test="$seg = ''">
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
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="o">
          <xsl:apply-templates select="@as"/>
          <xsl:apply-templates select="@name"/>
          <xsl:attribute name="base" select="concat('.', $seg)"/>
          <xsl:element name="o">
            <xsl:apply-templates select="$binding/@*[name() != 'as']"/>
            <xsl:apply-templates select="$binding/node()"/>
          </xsl:element>
          <xsl:apply-templates select="o"/>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!--
  Drop the standalone binding once it has been merged onto a reference.
  -->
  <xsl:template match="o[eo:moniker-binding(.) and eo:merges(.)]" priority="1"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
