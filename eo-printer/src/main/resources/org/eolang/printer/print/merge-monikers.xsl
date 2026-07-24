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
  formation (so the name still binds to the same formation). A bare `ξ.<name>`
  reference must be unnamed, but a single-segment dispatch `ξ.<name>.<seg>`
  (#5782) may also host onto a named reference (such as `q. > @`), since the
  reversed dispatch keeps the reference's own `@name` (#5794); a bare
  reference carrying arguments or a multi-segment path cannot host the binding
  and stays a reference. When no hostable reference exists, the binding is
  left in place.

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
  The single attribute name a hostable `ξ.<name>` reference resolves to, or
  the empty string for anything that is not hostable. Two shapes host a
  binding: a bare reference `ξ.<name>` (no trailing path, no arguments) and a
  single trailing dispatch segment `ξ.<name>.<seg>`, a reversed dispatch
  spelled forward whose receiver `<name>` can be inlined exactly as the bare
  case is, only as a reversed dispatch's receiver rather than a plain one
  (#5782). Both resolve to `<name>`. A multi-segment path or a non-`ξ` base
  disqualify it, as does a named node for the bare case only — a bare inline
  would clobber the reference's `@name`, but a dispatch keeps it (#5794).
  -->
  <xsl:function name="eo:resolved-ref" as="xs:string">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="tail" select="substring-after($ref/@base, concat($eo:xi, '.'))"/>
    <xsl:sequence select="if (exists($ref/@base) and not(exists($ref/@name)) and starts-with($ref/@base, concat($eo:xi, '.')) and not(contains($tail, '.')) and not($ref/o)) then $tail else if (eo:dispatch-seg($ref) != '') then substring-before($tail, '.') else ''"/>
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
  Whether `$attr` is a restored recursive `&gt;&gt; name` handle: an abstract
  formation carrying its readable "@local" handle whose visible "@name" has
  already been promoted to that same handle by "restore-local-names" (which
  sets "@name = @local" and rewrites the self-references only for a recursive
  formation, R-3.10.12 / #5681). Its cactus name is gone, yet when it is used
  as a method-dispatch receiver it must still host onto that dispatch as a
  reversed-dispatch moniker (#5848) — the recursive mirror of the non-recursive
  dispatch fold, where the anonymous formation is inlined (#5782). A void
  ("@base=∅"), a const handle (cactus "@name" ≠ readable "@local") and a plain
  cactus binding all fail "@name = @local", so only the restored recursive
  handle is admitted here.
  -->
  <xsl:function name="eo:recursive-handle" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:sequence select="eo:abstract($attr) and exists($attr/@local) and exists($attr/@name) and $attr/@name = $attr/@local"/>
  </xsl:function>
  <!--
  Whether `$attr` is a formation attribute eligible to become a moniker:
  auto-named with the cactus prefix (`a🌵`, §9.2), either bound (has a base)
  or an abstract formation (#5794), and neither void, `φ`, a test, nor already
  floated with a pipe (`|`). Only the compiler's obfuscated names are merged;
  a real, author-chosen name such as `value` reads best on its own
  `... > name` line and stays standalone (#5738). A restored recursive handle
  (see `eo:recursive-handle`) is the exception: its readable name is compiler
  plumbing that the non-recursive path discards entirely, so it too may host
  onto a dispatch use (#5848). Its bare uses always carry a `@name` (the
  `| ... > name` pipe #5837/#5848 leaves them with) and so never resolve as a
  hostable bare reference, so only a genuine dispatch use folds.
  -->
  <xsl:function name="eo:moniker-binding" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:sequence select="exists($attr/@name) and (starts-with($attr/@name, concat('a', $eo:cactoos)) or eo:recursive-handle($attr)) and (exists($attr/@base) or eo:abstract($attr)) and not(exists($attr/@pipe)) and not(eo:void($attr)) and $attr/@name != $eo:phi and not(eo:test-attr($attr)) and eo:abstract($attr/..)"/>
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
  The recursive-handle binding that an applied reference `$ref` hosts as a
  moniker, or the empty sequence. An applied reference is a bare "ξ.&lt;name&gt;"
  whose "&lt;name&gt;" is a restored recursive handle (see `eo:recursive-handle`)
  but which carries argument children — an application "handle args". Such a use
  hosts neither as a plain bare inline (that would silently drop the arguments,
  #5834) nor as a reversed dispatch, so instead the handle is inlined in place
  and the reference's arguments become a "| args" pipe continuation binding it
  (#5848), the recursive mirror of the applied-handle relocation "inline-cactoos"
  performs for the non-recursive case (#5844). Hosts onto the first such
  reference only; the binding's own subtree is excluded so a recursive
  self-reference is never mistaken for an external applied use. A reference
  carrying its own "@name" is excluded: that is a "| args &gt; name" sibling
  pipe continuation already folded by "restore-local-names" (#5837/#5848), not a
  receiver — the receiver has no result name, so this fold round-trips (the
  re-parsed "| args" reference is nameless and folds again).
  -->
  <xsl:function name="eo:applied-refs" as="element()*">
    <xsl:param name="attr" as="element()*"/>
    <xsl:variable name="owner" select="$attr/.."/>
    <xsl:sequence select="$owner//o[exists(@base) and starts-with(@base, concat($eo:xi, '.')) and substring-after(@base, concat($eo:xi, '.')) = $attr/@name and exists(o) and not(exists(@name)) and (ancestor::o[eo:abstract(.)][1] is $owner) and not(ancestor::o[. is $attr])]"/>
  </xsl:function>
  <xsl:function name="eo:applied-handle" as="element()*">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="owner" select="$ref/ancestor::o[eo:abstract(.)][1]"/>
    <xsl:variable name="name" select="substring-after($ref/@base, concat($eo:xi, '.'))"/>
    <xsl:variable name="binding" select="$owner/o[@name = $name and eo:moniker-binding(.) and eo:recursive-handle(.)][1]"/>
    <xsl:sequence select="if (exists($ref/@base) and starts-with($ref/@base, concat($eo:xi, '.')) and $name != '' and not(contains($name, '.')) and exists($ref/o) and exists($binding) and (eo:applied-refs($binding)[1] is $ref)) then $binding else ()"/>
  </xsl:function>
  <!--
  Whether `$attr` is a const file-local handle (`a &gt;&gt; b!`, R-3.10.12):
  a based binding (not an abstract formation) carrying both the `!` const
  marker and its readable "@local" handle. Such a handle is dataized once and
  cached, so a multi-referenced one is kept whole by "inline-cactoos" (#5828)
  and reaches here as an ordinary moniker binding. Unlike the non-const based
  handle (#5810), it keeps its obfuscated "@name" and "@local" when hosted, so
  "to-eo-tree" prints the merged binding as `a &gt;&gt; b!` rather than the
  anonymous `a!`.
  -->
  <xsl:function name="eo:const-handle" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:sequence select="exists($attr/@const) and exists($attr/@local) and not(eo:abstract($attr))"/>
  </xsl:function>
  <!--
  The const file-local handle binding a reference `$ref` resolves to but does
  NOT host (the binding folds onto its first reference only). Every other
  reference keeps the readable "@local" handle in place of the obfuscated
  cactus name, so it reads back as a bare `b` rather than a synthetic "vL_P"
  name (#5828).
  -->
  <xsl:function name="eo:kept-const-ref" as="element()*">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="owner" select="$ref/ancestor::o[eo:abstract(.)][1]"/>
    <xsl:variable name="binding" select="$owner/o[@name = eo:resolved-ref($ref) and eo:moniker-binding(.) and eo:const-handle(.)][1]"/>
    <xsl:sequence select="if (exists($binding) and not(eo:moniker-refs($binding)[1] is $ref)) then $binding else ()"/>
  </xsl:function>
  <!--
  Replace the first hosting reference with the merged binding, always keeping
  the reference's positional `@as`. A bare reference becomes the binding
  inlined in place: the binding's other attributes and its children. An
  abstract formation keeps its obfuscated `@name`, which `to-eo-tree` renders
  as the anonymous `[...] >>` marker; a based binding — a `>> name` handle
  whose value is a plain reference such as `a >> b` (R-3.10.12) — drops its
  `@name` (and the `@local` handle it leaves behind), since the bare reference
  is unnamed and carrying the obfuscated name over would turn the inline into
  a spurious named node that `to-eo-tree` prints as its own `a >>` line
  instead of an anonymous argument (#5810). A const based handle
  (`a >> b!`, see `eo:const-handle`) is the exception: it keeps its `@name` and
  `@local`, so `to-eo-tree` prints the readable `a >> b!` and its other
  references read back as the bare handle `b` (#5828). A single-segment dispatch
  `ξ.<name>.<seg>` becomes a reversed dispatch `<seg>.`
  whose receiver is that inlined binding and whose arguments are the
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
          <xsl:apply-templates select="$binding/@*[name() != 'as' and (eo:abstract($binding) or eo:const-handle($binding) or (name() != 'name' and name() != 'local'))]"/>
          <xsl:apply-templates select="$binding/node()"/>
        </xsl:element>
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
  Host an applied recursive handle (see `eo:applied-handle`): emit the inlined
  handle formation in place of the reference, then a "@pipe" node carrying the
  reference's arguments and pointing its base back at the formation's name.
  "to-eo-tree" renders the formation as the "&gt;&gt; name" receiver and the
  pipe node as "| args" because its base equals the preceding sibling's name —
  so an applied recursive handle folds to the handle moniker plus a pipe
  continuation carrying the arguments (#5848), the recursive mirror of #5844.
  The reference's own positional "@as" is kept on the pipe so an argument slot
  survives; the standalone binding is dropped below.
  -->
  <xsl:template match="o[exists(eo:applied-handle(.))]" priority="2">
    <xsl:variable name="binding" select="eo:applied-handle(.)"/>
    <xsl:for-each select="$binding">
      <xsl:copy>
        <xsl:apply-templates select="node()|@*"/>
      </xsl:copy>
    </xsl:for-each>
    <xsl:element name="o">
      <xsl:apply-templates select="@as"/>
      <xsl:attribute name="pipe"/>
      <xsl:attribute name="base" select="@base"/>
      <xsl:apply-templates select="o"/>
    </xsl:element>
  </xsl:template>
  <!--
  Rewrite a non-hosting reference to a const file-local handle from the
  obfuscated cactus name back to the readable "@local" handle, so it reads as
  `b` instead of a synthetic "vL_P" placeholder (#5828). The hosting reference
  is rebuilt from the binding above and never reaches this template.
  -->
  <xsl:template match="o[exists(eo:kept-const-ref(.))]/@base" priority="2">
    <xsl:variable name="binding" select="eo:kept-const-ref(..)"/>
    <xsl:attribute name="base" select="string-join(for $seg in tokenize(., '\.') return (if ($seg = $binding/@name) then string($binding/@local) else $seg), '.')"/>
  </xsl:template>
  <!--
  Drop the standalone binding once it has been merged onto a reference, whether
  the host is a bare/dispatch moniker reference (`eo:moniker-refs`) or an
  applied recursive handle folded to a "| args" pipe (`eo:applied-refs`, #5848).
  -->
  <xsl:template match="o[eo:moniker-binding(.) and (exists(eo:moniker-refs(.)) or (eo:recursive-handle(.) and exists(eo:applied-refs(.))))]" priority="1"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
