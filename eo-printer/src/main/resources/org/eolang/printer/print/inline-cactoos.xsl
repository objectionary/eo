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
  abstraction stay in place). A dataized-const handle (`a &gt;&gt; b!`,
  R-3.10.12) reached from more than one site is likewise left untouched:
  the const is dataized once and cached in that single binding, so folding
  it into each use would mint an independent const object per reference and
  drop the shared name (#5828); "merge-monikers" later hosts the kept
  binding onto its first reference.

  The inlined value keeps the target's obfuscated cactus `@name` only when
  the name is still meaningful downstream: an abstract formation, whose name
  `to-eo-tree` renders as the anonymous `[...] >>` marker, and a dataized-const
  wrapper (`.as-bytes` over `Φ.dataized`) over an abstract value, whose name
  `dataized-to-const` later promotes onto the abstract const node it rebuilds.
  A based `>> name` handle whose value is a plain reference or application
  (`a >> b`, R-3.10.12) is neither, so carrying its `@name` (or the `@local`
  handle) over would turn the inline into a spurious named node that
  `to-eo-tree` prints as its own `a >>` line, swallowing the surrounding
  argument; for such a target `@name` and `@local` are dropped and the value
  lands as an anonymous argument (#5810). A dataized-const wrapper over a
  non-abstract value is the anonymous inline const argument (`42.plus a!`,
  #5821): its cactus name is likewise dropped so the folded value reads inline
  as `a!`, not as a vertical `a >>!` line.
  -->
  <xsl:template match="o[contains(@base, concat('.', $auto))]" priority="0">
    <xsl:variable name="name" select="eo:resolved-name(@base)"/>
    <xsl:variable name="target" select="ancestor::o/o[@name=$name][1]"/>
    <xsl:variable name="keep-name" as="xs:boolean" select="exists($target) and (eo:abstract($target) or ($target/@base = '.as-bytes' and $target/o[1]/@base = 'Φ.dataized' and eo:abstract($target/o[1]/o[1])))"/>
    <xsl:choose>
      <xsl:when test="exists($target) and not(eo:void($target)) and not(eo:recursive($target, $name)) and not(eo:dataized-const($target) and eo:multi-referenced($target, $name))">
        <xsl:choose>
          <!--
          The reference is the base of an application — it carries its own
          argument children or a result-binding `@name`. Folding a fresh copy
          of the target formation over it (the bare-reference path below) would
          rebuild only the formation and silently drop those arguments and the
          name (#5834), the same class of loss as #5721. An abstract formation
          cannot be spelled inline as the head of an application, so instead of
          inlining it away we keep it in place as the auto-named `[] &gt;&gt;`
          predecessor (its binding is preserved by the drop template below) and
          turn this reference into the pipe continuation `| args &gt; name`
          that #5518 taught the printer to emit — an ordinary application node
          tagged `@pipe`, its `@base` still pointing at the kept formation
          directly above it. The guard keeps this to a reference standing
          immediately after its own target, the one shape `to-eo-tree` can
          render as a compact pipe (the formation stays put, kept by the drop
          template below).
          -->
          <xsl:when test="eo:abstract($target) and (o or @name) and preceding-sibling::o[1] is $target">
            <xsl:copy>
              <xsl:if test="not(@pipe)">
                <xsl:attribute name="pipe"/>
              </xsl:if>
              <xsl:apply-templates select="node()|@*"/>
            </xsl:copy>
          </xsl:when>
          <!--
          The same applied reference, but not standing as the formation's
          immediate following sibling, so the adjacent guard above misses it
          and the bare-reference `otherwise` would drop the argument and name
          exactly as #5834 did. Two shapes reach here: another binding sits
          between the formation and this use (`67 &gt; t` in #5840, the
          formation still a preceding sibling), or the use is a dispatch
          receiver buried as the `ρ` of a `.method` node (`(bar 55).a` in
          #5844), which shares no sibling slot with the formation at all. A `|`
          pipe binds the immediately-preceding sibling, so in both we relocate
          the single-use formation to sit directly above this reference — emit
          a fresh copy of it here (its children inlined as usual) and then the
          pipe continuation. For the receiver case the copy and the pipe land
          inside the dispatch block, so `to-eo-tree` renders them as the
          reversed dispatch's receiver (`[t] &gt;&gt; bar` then `| 55`). The
          formation's original binding is dropped by the drop template below (it
          is not `eo:piped`, having a non-reference following sibling, or no
          following sibling at all), so the relocation moves it rather than
          duplicating it, the same relocation #5732 proposes for its sibling
          case. Restricted to a single-use formation: a multi-referenced one
          cannot be folded into just one of its uses.
          -->
          <xsl:when test="eo:abstract($target) and (o or @name) and not(eo:multi-referenced($target, $name))">
            <xsl:for-each select="$target">
              <xsl:copy>
                <xsl:apply-templates select="node()|@*"/>
              </xsl:copy>
            </xsl:for-each>
            <xsl:copy>
              <xsl:if test="not(@pipe)">
                <xsl:attribute name="pipe"/>
              </xsl:if>
              <xsl:apply-templates select="node()|@*"/>
            </xsl:copy>
          </xsl:when>
          <xsl:otherwise>
            <xsl:element name="o">
              <xsl:if test="@as">
                <xsl:apply-templates select="@as"/>
              </xsl:if>
              <xsl:apply-templates select="$target/@*[$keep-name or (name() != 'name' and name() != 'local')]"/>
              <xsl:apply-templates select="$target/node()"/>
            </xsl:element>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="node()|@*"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!--
  Drop an inlined auto-named abstract; keep cactus-named voids, keep
  self-referential (recursive) abstracts, which are never inlined, keep a
  multi-referenced dataized-const handle, which is never inlined (#5828), keep
  a formation applied through a `@pipe` continuation, which is kept in place
  above its pipe rather than inlined (#5834), and keep a binding that a
  surviving method dispatch still reaches through its name.
  Such a dispatch reference (`ξ.<name>.<seg>`) is not inlined above — its
  receiver is buried in a dotted base — so dropping the binding would strand
  the reference on a synthetic "vL_P" placeholder. Keeping it lets
  "merge-monikers" host the binding as the receiver of a reversed dispatch
  instead (#5782).
  -->
  <xsl:template match="o[starts-with(@name, $auto) and not(eo:void(.))]" priority="1">
    <xsl:if test="eo:recursive(., @name) or eo:dispatched(., @name) or (eo:dataized-const(.) and eo:multi-referenced(., @name)) or eo:piped(., @name)">
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
  <!--
  Whether a reference in the auto-named binding's owner reaches it through a
  method dispatch `ξ.<name>.<seg>` — its base carries the binding's name
  followed by a further segment. Such a reference is not inlined (its receiver
  is not a bare cactus name), so the binding is kept for "merge-monikers"
  rather than dropped (#5782). The binding's own subtree is excluded so a
  helper that merely dispatches on itself is not mistaken for an external use.
  -->
  <xsl:function name="eo:dispatched" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="exists($target/..//o[contains(@base, concat($name, '.')) and not(ancestor-or-self::o[. is $target])])"/>
  </xsl:function>
  <!--
  Whether the auto-named binding `$target` is a dataized-const wrapper: a
  `.as-bytes` node over a `Φ.dataized` node, the shape "const-to-dataized"
  leaves behind for a const file-local `&gt;&gt; name!` handle (R-3.10.12).
  Such a const is dataized once and its result cached in that single binding,
  so every reference shares one const object.
  -->
  <xsl:function name="eo:dataized-const" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:sequence select="$target/@base = '.as-bytes' and $target/o[1]/@base = 'Φ.dataized'"/>
  </xsl:function>
  <!--
  Whether the auto-named abstract formation `$target` is immediately followed
  by a reference that uses it as the base of an application — a sibling
  resolving to `$name` that carries its own argument children or a
  result-binding `@name`. Such a reference is not inlined away (see the
  inlining template) but rewritten into a `| args &gt; name` pipe continuation
  pointing back at the formation, so the formation must stay in place as the
  pipe's named predecessor rather than be dropped (#5834). The bare-reference
  case (no children, no name) folds the formation in and drops it as before.
  An applied reference separated from the formation by another binding (#5840)
  is deliberately not matched here: the inlining template relocates a fresh
  copy of the formation down to the reference's site, so the original binding
  is dropped by the drop template above rather than kept in place.
  -->
  <xsl:function name="eo:piped" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:variable name="next" select="$target/following-sibling::o[1]"/>
    <xsl:sequence select="eo:abstract($target) and exists($next) and contains($next/@base, concat('.', $auto)) and eo:resolved-name($next/@base) = $name and ($next/o or $next/@name)"/>
  </xsl:function>
  <!--
  Whether more than one reference in the binding's owner resolves to the
  auto-name `$name` (references inside the binding's own subtree excluded).
  A const binding reached from several sites must stay a single shared handle
  rather than be folded into each use: inlining it would mint an independent
  const object per reference, changing the object graph and dropping the
  shared name (#5828). "merge-monikers" then hosts the kept binding onto its
  first reference.
  -->
  <xsl:function name="eo:multi-referenced" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="count($target/..//o[contains(@base, concat('.', $auto)) and eo:resolved-name(@base) = $name and not(ancestor-or-self::o[. is $target])]) &gt; 1"/>
  </xsl:function>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
