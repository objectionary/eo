<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs eo" id="inline-cactoos" version="2.0">
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
  The dotted cactus prefix, hoisted out of the hot functions below: "$auto"
  is a global variable rather than a literal, so Saxon rebuilds the same
  concatenation at every call instead of folding it (#8529).
  -->
  <xsl:variable name="auto-dot" select="concat('.', $auto)"/>
  <!--
  Every reference to a cactus name, by the name it resolves to and by the
  head segment of that name, which is the same thing for a bare reference
  ("ξ.ρ.a🌵4-2") and the receiver for a method dispatch
  ("ξ.ρ.a🌵4-2.seg"). The functions below used to answer "which references
  name this binding?" with a "$target/..//o[...]" subtree scan, reached from
  template patterns Saxon evaluates against every "o" node, which made a
  print quadratic in the size of the largest formation (#8529). The indexes
  answer the same question without walking anything, as in "merge-monikers"
  (#6511); the scoping stays where it was, as a predicate on what the index
  hands back, and keys answer in document order, so "the first reference"
  and "the second one" keep their meaning.
  -->
  <xsl:key name="local-ref" match="o[contains(@base, $auto)]" use="eo:resolved-name(@base)"/>
  <xsl:key name="local-head" match="o[contains(@base, $auto)]" use="substring-before(concat(eo:resolved-name(@base), '.'), '.')"/>
  <!--
  A reference resolves to its own auto-name: given a base such as
  `ξ.ρ.a🌵4-2`, everything up to the cactus prefix is stripped, so the
  resolved name is the trailing `a🌵4-2`. This mirrors the `$name`
  computation in the inlining template below.

  Every call site hands over the `@base` attribute node itself, and it is turned
  into a string here rather than declared as `xs:string` and left to the function
  conversion rules (#6669). Saxon may bind a parameter to a closure over the
  argument expression instead of over its converted value, and the node then
  arrives in the body, where an expression compiled on the strength of a
  statically atomic parameter (the `substring-before` below, or the
  `ValueComparison` against `$name` in `eo:references`) casts it straight to an
  atomic value and kills the whole sheet with `DOMNodeWrapper cannot be cast to
  AtomicValue`. The `[2]` of #6638 is what made that reachable: it moved such a
  predicate into a lazy subscript, which is exactly the context where the
  deferred conversion is lost. Nothing is read differently for it: `string()` of
  an attribute node is the value the conversion rules would have produced, and a
  node carrying no `@base` at all now resolves to the empty string, which no
  reference to an auto-name can equal, rather than raising a type error.
  -->
  <xsl:function name="eo:resolved-name" as="xs:string">
    <xsl:param name="base" as="item()?"/>
    <xsl:variable name="text" as="xs:string" select="string($base)"/>
    <xsl:sequence select="substring-after($text, substring-before($text, $auto))"/>
  </xsl:function>
  <!--
  The based `&gt;&gt; name` handle that `$target` ultimately denotes, chasing a
  chain of bare-reference aliases to its end. A handle whose value is a plain
  reference to another auto-named handle (`p &gt;&gt; r` over `E0- &gt;&gt; p`)
  carries no value of its own — it is a transparent alias — so folding it into a
  use site must land the real value (`E0-`), not the reference to the next
  handle in the chain. Each hop follows a `@base` that resolves to another
  auto-named handle, the current handle carrying no arguments of its own, and
  stops at the first handle whose `@base` is not such a reference (the one that
  holds the real value), or at one carrying its own children, an abstract
  formation or a void — none of which is a transparent alias. A target that is
  not an alias resolves to itself. Without this the alias's own `@base` is copied
  to the use site verbatim while the drop template removes the handle it names,
  stranding a dangling `Φ.v..` (#6004). The chain travelled so far comes along in
  `$seen`, since two bare aliases can reference each other (`p &gt;&gt; r` beside
  `r &gt;&gt; p`) — source the parser accepts though it never dataizes — and a
  repeat has to end the descent rather than recur forever, exactly as the
  "merged" mode of "merge-monikers" guards its own handle chain (#5918).
  -->
  <xsl:function name="eo:alias-target" as="element()">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="seen" as="element()*"/>
    <xsl:variable name="inner" select="$target/ancestor::o/o[@name=eo:resolved-name($target/@base)][1]"/>
    <xsl:sequence select="if (contains($target/@base, $auto-dot) and not($target/o) and exists($inner) and not(eo:void($inner)) and not(eo:abstract($inner)) and (every $node in $seen satisfies not($inner is $node))) then eo:alias-target($inner, ($seen, $target)) else $target"/>
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
  <xsl:template match="o[contains(@base, $auto-dot)]" priority="0">
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
          unparsable `|:N` (#5983). Leave the handle standing under its
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
          The same applied reference, but written inside a nested formation
          body rather than in the handle's own owner scope. Such a reference
          reaches the handle as `ξ.ρ.` (a `ρ` climb into the enclosing scope
          where the handle lives), so the relocation branch below would land
          the fresh formation copy in the nested scope while the pipe node
          keeps the `ρ` climb: `to-eo-tree`'s adjacency test then sees the
          pipe's `ξ.ρ.name` base no longer equal `ξ.name` of the copy directly
          above it and prints the node as an ordinary application on the cactus
          name, leaving the copy a stray extra argument and orphaning the
          reference (#6021). An argument list buried in a nested scope has no
          more room for a relocated predecessor than the positional-argument
          case above (#5983), so leave the handle standing under its `@local`
          name instead — copy the reference verbatim, keeping its arguments, so
          the drop template below keeps the binding in its own scope and
          "merge-monikers" rewrites the reference back to the handle
          (`eo:kept-local-ref`), shedding the `ρ` climb (#5893/#5917). Gated on
          the reference's nearest formation ancestor not being the handle's
          owner, so the sibling (#5840) and receiver (#5844) relocations below,
          whose references share the handle's own scope, still fire.
          -->
          <xsl:when test="eo:abstract($target) and (o or @name) and not(eo:multi-referenced($target, $name)) and not(ancestor::o[eo:abstract(.)][1] is $target/..)">
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
          reference is handled by the branch above (#5983), and a
          nested-scope reference by the branch just above (#6021), not here.
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
            <!--
            A based `&gt;&gt; name` handle whose value is a bare reference to
            another based handle (`p &gt;&gt; r` over `E0- &gt;&gt; p`) is a
            transparent alias: copying `$target`'s `@base` verbatim would carry
            the reference to `p` down to the use site, while the drop template
            below removes `p`'s binding — `p` counts as referenced by `r`, yet
            that reference now lives only in the copy — stranding a dangling
            `Φ.v..` that nothing declares (#6004). Resolving the alias chain to
            its end (see `eo:alias-target`) instead lands the real value (`E0-`)
            at the site in one step, exactly as `merge-monikers` carries a whole
            chain of handles to the site it hosts them on (#5918). A target that
            is not such an alias resolves to itself, so this is the unchanged
            fold for the ordinary based handle.
            -->
            <xsl:variable name="value" select="eo:alias-target($target, ())"/>
            <o>
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
              <!--
              The folded value is rebased as a whole `o` and then unwrapped
              into the node being built, rather than as the loose sequence of
              its attributes and children. The `dropped` mode rewrites an
              `@base` only as it walks the `o` carrying it, so a bare sequence
              hands it the value's own `@base` as a stray attribute node that
              the catch-all copies verbatim: `x.plus 1 &gt;&gt; h` folded into a
              nested formation kept reading `ξ.x`, which is that formation's own
              `x` and not the one the handle was written against (#7097). The
              wrapper also carries whether the value is a formation, so its
              children are counted one level deeper, the way any nested
              formation is (see `eo:dropped-base`).
              -->
              <xsl:variable name="folded" as="element()">
                <o>
                  <xsl:apply-templates select="$value/@*[$keep-name or (name() != 'name' and name() != 'local')]"/>
                  <xsl:apply-templates select="$value/node()"/>
                </o>
              </xsl:variable>
              <xsl:variable name="rebased" as="element()">
                <xsl:apply-templates select="$folded" mode="dropped">
                  <xsl:with-param name="drop" select="eo:host-drop($target, .)" tunnel="yes"/>
                </xsl:apply-templates>
              </xsl:variable>
              <xsl:copy-of select="$rebased/@*"/>
              <xsl:copy-of select="$rebased/node()"/>
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
              <xsl:if test="not($value/o)">
                <xsl:apply-templates select="o"/>
              </xsl:if>
            </o>
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
  single-use formation reached only through a reference in a nested formation
  scope (see `eo:nested-applied`), which is left standing rather than relocated
  into that scope where its pipe's `ρ` climb would orphan the reference
  (#6021), keep a based application handle reached by a further
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

  The handle name is atomised once here, for the reason spelled out on
  `eo:resolved-name` above (#6669). Each of the nine questions below declares its
  `$name` as `xs:string`, and this template is the only place that answered them
  with the `@name` attribute node rather than its string value — every other
  caller passes the `$name` variable of the inlining template, itself the string
  returned by `eo:resolved-name`. Handing over the node is what let a
  `DOMNodeWrapper` reach the `ValueComparison` inside `eo:references` and crash
  the whole sheet. The match pattern requires `@name`, so the string is always
  the name the nine were asked about before.
  -->
  <xsl:template match="o[starts-with(@name, $auto) and not(eo:void(.))]" priority="1">
    <xsl:variable name="name" as="xs:string" select="string(@name)"/>
    <xsl:if test="eo:kept-binding(., $name)">
      <!--
      This kept binding's own value may itself be a bare reference to
      another based handle (`p >> r` over `E0- >> p`, #7297) — a
      transparent alias exactly like the one `eo:alias-target` resolves
      for an ordinary reference above. When the aliased handle is not
      itself kept (none of the same nine conditions apply to it), the
      priority-0 template above never runs for it either — it never
      matches this element, whose higher-priority binding-drop match
      wins — so its binding vanishes and a naive verbatim copy would
      leave this node's `@base` pointing at nothing, printed as an
      orphaned synthetic name (#7297). Resolve the alias chain here too,
      landing the real value directly, and skip this for an abstract or
      void alias target, whose own name (or const layout) still matters
      and is handled by the ordinary reference path instead.
      -->
      <xsl:variable name="ref-name" select="if (contains(@base, $auto-dot)) then eo:resolved-name(@base) else ()"/>
      <xsl:variable name="ref-target" select="if (exists($ref-name)) then ancestor::o/o[@name=$ref-name][1] else ()"/>
      <xsl:choose>
        <xsl:when test="exists($ref-target) and not(eo:void($ref-target)) and not(eo:abstract($ref-target)) and not(eo:kept-binding($ref-target, $ref-name))">
          <xsl:variable name="alias" select="eo:alias-target($ref-target, ())"/>
          <xsl:copy>
            <xsl:apply-templates select="@*[name() != 'base']"/>
            <xsl:attribute name="base" select="$alias/@base"/>
            <xsl:apply-templates select="$alias/node()"/>
          </xsl:copy>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy>
            <xsl:apply-templates select="node()|@*"/>
          </xsl:copy>
        </xsl:otherwise>
      </xsl:choose>
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
    <xsl:sequence select="exists(key('local-ref', $name, root($target))[contains(@base, $auto-dot)][ancestor::*[. is $target]])"/>
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
    <xsl:sequence select="exists(key('local-head', $name, root($target))[ancestor::*[. is $target/..]][contains(@base, concat($name, '.'))][not(ancestor-or-self::o[. is $target])])"/>
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
    <xsl:sequence select="eo:dataized-const($target) and not(eo:abstract($target/o[1]/o[1])) and exists($target//o[contains(@base, $auto-dot)])"/>
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
    <xsl:sequence select="eo:abstract($target) and exists($next) and contains($next/@base, $auto-dot) and eo:resolved-name($next/@base) = $name and ($next/o or $next/@name)"/>
  </xsl:function>
  <!--
  Whether the single-use auto-named abstract formation `$target` is applied by a
  reference that stands in a positional argument slot of an application — a
  reference resolving to `$name`, carrying its own argument children or a
  result-binding `@name`, whose own positional `@as` is `αN`. Unlike the sibling
  (#5840) and receiver (#5844) relocations, an argument list has no spare slot
  for the formation copy a `| args` pipe binds: relocating there would turn the
  copy into a stray extra argument and leave the pipe carrying the reference's
  `@as`, which `to-eo-tree` spells as the unparsable `|:N` (#5983). The
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
  Whether the single-use auto-named abstract formation `$target` is applied by a
  reference written inside a nested formation body rather than in the handle's
  own owner scope. Such a reference reaches the handle through a `ρ` climb
  (`ξ.ρ.<name>`) into the enclosing scope where the handle lives. Relocating the
  formation copy down to the reference (the #5840/#5844 branch) would drop it
  into the nested scope while the pipe node keeps the `ρ` climb, so
  `to-eo-tree`'s adjacency test no longer matches the copy directly above the
  pipe and the node prints as an ordinary application, leaving a stray copy and
  an orphaned reference (#6021). The reference is therefore left standing (see
  the inlining template) and this binding must be kept in place under its
  `@local` name rather than relocated, so "merge-monikers" can rewrite the
  reference back to the handle (`eo:kept-local-ref`), shedding the `ρ` climb
  (#5893/#5917). A multi-referenced formation is excluded — it is already kept
  whole by the outer guard above — and references inside the formation itself
  are not counted. The nearest formation ancestor of the reference is compared
  against the handle's owner (`$target/..`): the sibling (#5840) and receiver
  (#5844) relocations, whose references share the handle's own scope, are
  deliberately not matched here.
  -->
  <xsl:function name="eo:nested-applied" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="eo:abstract($target) and not(eo:multi-referenced($target, $name)) and exists(eo:references($target, $name)[eo:resolved-name(@base) = $name and (o or @name) and not(ancestor::o[eo:abstract(.)][1] is $target/..)])"/>
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
  Whether the auto-named binding `$target` survives the drop template
  below — the same nine questions the template's own "xsl:if" asks,
  shared so a binding's kept/dropped status can be looked up for a
  target other than the current node (#7297).
  -->
  <xsl:function name="eo:kept-binding" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="eo:recursive($target, $name) or eo:dispatched($target, $name) or eo:vertical-const($target) or eo:unreferenced($target, $name) or (eo:multi-referenced($target, $name) and eo:rebuilt($target)) or eo:piped($target, $name) or eo:arg-applied($target, $name) or eo:nested-applied($target, $name) or eo:reapplied($target, $name)"/>
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
    <xsl:sequence select="key('local-head', $name, root($target))[contains(@base, $auto-dot)][ancestor::*[. is $target/..]][not(ancestor-or-self::o[. is $target])]"/>
  </xsl:function>
  <!--
  Whether more than one reference in the binding's owner reaches the auto-name
  `$name`. A shared binding that is rebuilt at every site (see `eo:rebuilt`) is
  kept whole rather than folded into each use, which would change the object
  graph and drop the shared handle name, and "merge-monikers" then hosts the
  kept binding onto its first reference.

  The second item is asked for instead of the size (#6638): Saxon may back the
  sequence with a "MemoSequence", whose iterator refuses "getLength()", so
  counting it crashes the whole sheet on the runs where the optimiser picks
  that representation. Asking for "[2]" answers the same question, never counts
  and short-circuits, the way the neighbouring "eo:unreferenced" already leans
  on "empty()".
  -->
  <xsl:function name="eo:multi-referenced" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="exists(eo:references($target, $name)[2])"/>
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
  <!--
  How many formations `$host` sits below `$bound`. A value folded onto a
  reference deeper than the binding it came from is read again in a scope
  that is not the one it was written in, so the names in it that reach out
  of the value have to climb that difference (#7095).
  -->
  <xsl:function name="eo:host-drop" as="xs:integer">
    <xsl:param name="bound" as="element()"/>
    <xsl:param name="host" as="element()"/>
    <xsl:sequence select="count($host/ancestor::o[eo:abstract(.)]) - count($bound/ancestor::o[eo:abstract(.)])"/>
  </xsl:function>
  <!--
  The leading run of `ρ` segments, which is how far a base climbs before
  it names anything.
  -->
  <xsl:function name="eo:rho-climb" as="xs:integer">
    <xsl:param name="segments" as="xs:string*"/>
    <xsl:sequence select="if (empty($segments) or $segments[1] != $eo:rho) then 0 else 1 + eo:rho-climb(subsequence($segments, 2))"/>
  </xsl:function>
  <!--
  The base `$base` as it must read after the value carrying it dropped
  `$drop` formations, from a node `$depth` formations inside that value.
  A climb of `$depth` lands on the value's own root, so a climb that far
  or further has left the value and gains `$drop` hops. A shorter climb
  stays inside, and a base rooted anywhere but `ξ`, or naming nothing
  past its climb, is left alone.
  -->
  <xsl:function name="eo:dropped-base" as="xs:string">
    <xsl:param name="base" as="xs:string"/>
    <xsl:param name="drop" as="xs:integer"/>
    <xsl:param name="depth" as="xs:integer"/>
    <xsl:variable name="segments" select="tokenize($base, '\.')"/>
    <xsl:variable name="climb" select="if ($segments[1] = $eo:xi) then eo:rho-climb(subsequence($segments, 2)) else -1"/>
    <xsl:sequence select="if ($drop &lt;= 0 or $climb &lt; $depth or count($segments) &lt;= $climb + 1) then $base else string-join(($eo:xi, for $i in 1 to ($climb + $drop) return $eo:rho, subsequence($segments, $climb + 2)), '.')"/>
  </xsl:function>
  <!--
  Rewrites the bases of a folded value, counting how deep inside it each
  node sits so that a name reaching no further than the value itself is
  left where it is. The based-handle fold is the only site that needs it,
  and #7097 asked why. Nothing else in the print train moves a value across
  a formation boundary: the four branches above it either leave the handle
  standing and copy the reference in place (#5983, #6021, #5834), or
  relocate the handle onto a reference the guards have already restricted to
  the handle's own scope, since the nested ones are taken by the branch
  above (#6021). "merge-monikers" is the same story from the other side —
  each of its three merges keys its host reference by the reference's
  nearest formation ancestor and looks the binding up under that same
  formation, so a host always shares the binding's scope and the drop is
  zero. Only a handle reached from a nested scope travels, and only the fold
  below carries it there.
  -->
  <xsl:template match="o" mode="dropped">
    <xsl:param name="drop" as="xs:integer" tunnel="yes"/>
    <xsl:param name="depth" as="xs:integer" select="0"/>
    <xsl:copy>
      <xsl:copy-of select="@*[name() != 'base']"/>
      <xsl:if test="@base">
        <xsl:attribute name="base" select="eo:dropped-base(@base, $drop, $depth)"/>
      </xsl:if>
      <xsl:apply-templates select="node()" mode="dropped">
        <xsl:with-param name="depth" select="if (eo:abstract(.)) then $depth + 1 else $depth"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*" mode="dropped">
    <xsl:param name="depth" as="xs:integer" select="0"/>
    <xsl:copy>
      <xsl:apply-templates select="node()|@*" mode="dropped">
        <xsl:with-param name="depth" select="$depth"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
