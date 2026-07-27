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
  abstraction stay in place). A target that is rebuilt at every site it lands
  in (see `eo:rebuilt`) and reached from more than one site is likewise left
  untouched: folding it into every use copies one shared object into several
  and drops the handle name they all read. A dataized-const handle
  (`a &gt;&gt; b!`, R-3.10.12) is dataized once and cached in that single
  binding, so a per-use fold would mint an independent const object per
  reference (#5828); an abstract formation would strand any sibling helper it
  reaches on a synthetic "vL_P" id (#5876); and a based handle over an
  application such as `a.plus 1 &gt;&gt; b` would print the same application
  twice, folded into the bare reference and left standing for the dispatch
  ones, which are never inlined (#5956). The binding and its "@local" handle
  stay in place and "merge-monikers" later hosts them
  onto the first reference. A const
  whose value reaches another auto-name is kept for a third reason (#5910):
  the folded value can only be laid out vertically, which the anonymous inline
  const argument cannot spell (see "eo:vertical-const" below).

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
      <xsl:when test="exists($target) and not(eo:void($target)) and not(eo:recursive($target, $name)) and not(eo:vertical-const($target)) and not(eo:multi-referenced($target, $name) and eo:rebuilt($target)) and not(eo:reapplied($target, $name))">
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
          The same applied reference standing in a positional argument slot
          (`@as` is `αN`) of an application, rather than in a body-binding
          sibling slot (#5840) or a dispatch receiver (#5844). The relocation
          branch below would emit the formation copy and the `| args` pipe as
          two children in place of this reference, but an argument list has no
          room for the relocated predecessor a pipe binds: the copy would become
          a stray extra argument of the application and the pipe would keep this
          reference's positional `@as`, which `to-eo-tree` spells as the
          unparseable `|:N` (#5983). Leave the handle standing under its
          `@local` name instead — copy the reference verbatim, keeping its
          arguments, so it reads back as `bar 1 2` once the drop template below
          keeps the binding and "merge-monikers" rewrites its cactus base to the
          handle (`eo:kept-local-ref`), the kept-handle path of #5876 and #5944.
          Restricted to a single-use formation reached through a positional
          argument: the sibling and receiver relocations still fire for #5840
          and #5844, whose references carry no positional `@as`.
          -->
          <xsl:when test="eo:arg-applied($target, $name) and (o or @name) and starts-with(@as, $eo:alpha)">
            <xsl:copy>
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
          cannot be folded into just one of its uses. A positional-argument
          reference is handled by the branch above (#5983), not here.
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
              <!--
              The reference carries a binding name of its own and the target
              contributes none — a based `a &gt;&gt; b` handle is neither an
              abstract formation nor a dataized const, so `$keep-name` is false
              and its own name is dropped (#5810). Folding then copies only the
              target's attributes, and the reference's `@name` would go with the
              fold: `to-eo-tree` prints the value as a nameless body line, which
              the parser rejects with "object inside formation must have a name".
              A `φ` decoratee written ahead of the brackets
              (`false &gt; [] &gt;&gt;`, #5882) loses its head that way and an
              ordinary binding (`b &gt; x`, #5947) loses its whole name, so the
              reference's name is carried over in both cases. Never for a target
              that keeps a name of its own: an abstract formation prints its
              `[...] &gt;&gt;` marker instead, and the two would collide.
              -->
              <xsl:if test="@name and not($keep-name)">
                <xsl:apply-templates select="@name"/>
              </xsl:if>
              <xsl:apply-templates select="$target/@*[$keep-name or (name() != 'name' and name() != 'local')]"/>
              <xsl:apply-templates select="$target/node()"/>
              <!--
              The reference is the base of an application (`b 42 &gt; x` over a
              based `a.plus &gt;&gt; b` handle), so it carries its own argument
              children. Folding only the target's attributes and children —
              emitting `a.plus` alone — silently drops those arguments and prints
              `a.plus &gt; x` (#5952), the based flavour of the loss #5834 and
              #5887 fixed for the abstract and named shapes. The reference's own
              `o` children are appended after the target's so the `42` survives as
              `a.plus 42 &gt; x`. Guarded to a target that carries no children of
              its own: appending to an already-applied target (`a.plus 1 &gt;&gt;
              b` used as `b 42`) would hand the inner application a second
              argument rather than apply its result, so such a target has no
              inline spelling here and is left standing above instead (see
              `eo:reapplied`).
              -->
              <xsl:if test="not($target/o)">
                <xsl:apply-templates select="o"/>
              </xsl:if>
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
  binding no reference reaches at all (#5914), keep a multi-referenced binding
  that is rebuilt at every site — a dataized-const handle (#5828), an abstract
  formation (#5876) or an application (#5956) — which is never
  inlined, keep a const that only a vertical layout can spell,
  which is never inlined either (#5910), keep a formation applied through a
  `@pipe` continuation, which is kept in place above its pipe rather than
  inlined (#5834), keep a single-use formation reached through a positional
  argument (see `eo:arg-applied`), which is left standing rather than relocated
  into an argument list where its pipe would print as `|:N` (#5983), keep a
  based application handle reached by a further
  application (see `eo:reapplied`), which has no inline spelling as the head of
  another application and is left standing rather than folded (#5952), and keep
  a binding that a surviving method dispatch still reaches through its name.
  Such a dispatch reference (`ξ.<name>.<seg>`) is not
  inlined above — its receiver is buried in a dotted base — so dropping the
  binding would strand the reference on a synthetic "vL_P" placeholder. Keeping
  it lets "merge-monikers" host the binding as the receiver of a reversed
  dispatch instead (#5782). An unreferenced binding is kept because inlining
  never moved its value anywhere: dropping it deletes the declaration from the
  printed source, so a private helper formation or const cache that nothing
  reads yet would silently vanish (#5914).
  -->
  <xsl:template match="o[starts-with(@name, $auto) and not(eo:void(.))]" priority="1">
    <xsl:if test="eo:recursive(., @name) or eo:dispatched(., @name) or eo:vertical-const(.) or eo:unreferenced(., @name) or (eo:multi-referenced(., @name) and eo:rebuilt(.)) or eo:piped(., @name) or eo:arg-applied(., @name) or eo:reapplied(., @name)">
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
  Whether folding the dataized-const `$target` would demand a vertical
  spelling that the anonymous inline const argument cannot carry. A const
  over a non-abstract value folds as the nameless `42.plus a!` argument
  (#5821), whose `!` rides the head of one line; a const over an abstract
  value keeps its cactus name and prints as the multi-line `[] &gt;&gt;!`
  instead, so only the nameless flavour is at stake. When such a value
  reaches another auto-name, "merge-monikers" later hosts that binding
  inside the folded value as its own `&gt;&gt; name` line, and the value can
  then only be laid out vertically — leaving the `!` right behind the head,
  as in `if.!`, which R-3.8.1 rejects, so the next parse silently drops the
  whole body (#5910). Such a const is kept as its own named binding instead,
  the same carve-out the multi-referenced const gets (#5828).
  -->
  <xsl:function name="eo:vertical-const" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:sequence select="eo:dataized-const($target) and not(eo:abstract($target/o[1]/o[1])) and exists($target//o[contains(@base, concat('.', $auto))])"/>
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
  Whether the single-use auto-named abstract formation `$target` is applied by a
  reference that stands in a positional argument slot of an application — a
  reference resolving to `$name`, carrying its own argument children or a
  result-binding `@name`, whose own positional `@as` is `αN`. Unlike the sibling
  (#5840) and receiver (#5844) relocations, an argument list has no spare slot
  for the formation copy a `| args` pipe binds: relocating there would turn the
  copy into a stray extra argument and leave the pipe carrying the reference's
  `@as`, which `to-eo-tree` spells as the unparseable `|:N` (#5983). The
  reference is therefore left standing (see the inlining template) and this
  binding must be kept in place under its `@local` name rather than dropped, so
  "merge-monikers" can rewrite the reference back to the handle
  (`eo:kept-local-ref`, the kept-handle path of #5876 and #5944). A
  multi-referenced formation is excluded — it is already kept whole by the outer
  guard above — and references inside the formation itself are not counted.
  -->
  <xsl:function name="eo:arg-applied" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="eo:abstract($target) and not(eo:multi-referenced($target, $name)) and exists(eo:references($target, $name)[eo:resolved-name(@base) = $name and (o or @name) and starts-with(@as, $eo:alpha)])"/>
  </xsl:function>
  <!--
  Whether the based `&gt;&gt; name` handle `$target` is itself an application
  carrying its own argument children (`a.plus 1 &gt;&gt; b`) and is reached by a
  reference that applies it further (`b 42`). The otherwise branch of the
  inlining template folds a based handle by copying the target's own children
  and then appending the reference's — which for a target that already carries
  arguments would hand the inner application a second one rather than apply its
  result. Such a target has no inline spelling as the head of another
  application, so both the reference and the binding are left standing rather
  than folded (#5952). Restricted to based handles: an abstract formation
  becomes a `| args` pipe (see the two abstract branches above), and a bare or
  argument-less based handle (`a.plus &gt;&gt; b`) carries no children of its
  own, so appending the reference's arguments applies it cleanly.
  -->
  <xsl:function name="eo:reapplied" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="not(eo:abstract($target)) and not(eo:dataized-const($target)) and exists($target/o) and exists(eo:references($target, $name)[eo:resolved-name(@base) = $name and o])"/>
  </xsl:function>
  <!--
  The references in the binding's owner that reach the auto-name `$name`
  (references inside the binding's own subtree excluded). A reference reaches
  `$name` either bare (`ξ.<name>`) or through a method dispatch
  (`ξ.<name>.<seg>`, whose resolved name carries the bare name as its leading
  segment); both are collected. A reference from inside a nested formation body
  reaches it as `ξ.ρ.<name>`, which `eo:resolved-name` strips down to the same
  auto-name, so it is counted like any other.
  -->
  <xsl:function name="eo:references" as="element()*">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="$target/..//o[contains(@base, concat('.', $auto)) and (eo:resolved-name(@base) = $name or starts-with(eo:resolved-name(@base), concat($name, '.'))) and not(ancestor-or-self::o[. is $target])]"/>
  </xsl:function>
  <!--
  Whether more than one reference in the binding's owner reaches the auto-name
  `$name`. A shared binding that is rebuilt at every site (see `eo:rebuilt`) is
  kept whole rather than folded into each use, which would change the object
  graph and drop the shared handle name, and "merge-monikers" then hosts the
  kept binding onto its first reference.
  -->
  <xsl:function name="eo:multi-referenced" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="count(eo:references($target, $name)) &gt; 1"/>
  </xsl:function>
  <!--
  Whether `$target` is built anew at every site it is folded into, so that
  folding a shared one into each of its uses turns one object into several. An
  abstract formation (#5876), a dataized const (#5828) and an application such
  as `a.plus 1` (#5956) all are: each copy is its own object. A based handle
  whose value is a bare reference (`a &gt;&gt; b`) or an argument-less dispatch
  (`a.as-i32 &gt;&gt; b`) is not — it is a lookup, and every copy reads back the
  very same object — so a shared one still folds per use and its name goes with
  it (#5810).
  -->
  <xsl:function name="eo:rebuilt" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:sequence select="eo:abstract($target) or exists($target/o)"/>
  </xsl:function>
  <!--
  Whether no reference in the binding's owner reaches the auto-name `$name`.
  Such a binding has no use site to fold into, so it is kept where it stands
  rather than dropped (#5914).
  -->
  <xsl:function name="eo:unreferenced" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="empty(eo:references($target, $name))"/>
  </xsl:function>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
