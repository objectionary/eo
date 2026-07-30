<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="restore-local-names" version="2.0">
  <!--
  Inverse of the parser's "resolve-local-names" pass, applied before
  printing (#5563). A void declared with a file-local handle
  ("? &gt;&gt; name", R-3.10.12) keeps a synthetic cactus @name plus a
  "@local='name'" marker; the parser resolves references to the cactus name
  but the readable handle is preserved on the void. Here we put the handle
  back: the void's @name becomes its handle and every @base segment that
  points at that cactus name is rewritten to the handle, so references read
  under the handle instead of a synthetic "vL_P" placeholder. The
  "@local" marker is deliberately KEPT on the void (#5581) so that
  "to-eo-tree" can print it back as a vertical "? &gt;&gt; name" line and
  preserve the void's anonymity (§9.2, R-9.2.3), rather than collapsing it
  into a public "[name]" bracket param.

  A non-void "&gt;&gt;" handle sits on an anonymous formation. Usually the
  "inline-cactoos" pass inlines that formation away, so its handle is
  irrelevant and the "@local" marker is simply dropped. A self-referential
  (recursive) formation is the exception: since #5677 "inline-cactoos"
  correctly keeps it in place, so it reaches "to-eo-tree" and must carry a
  readable name. Using the same self-reference test as that guard, such a
  formation is treated like a handled void here — its "@local" is promoted
  to the visible "@name", its self-references are rewritten back to the
  handle, and the marker is KEPT so "to-eo-tree" prints "&gt;&gt; name"
  rather than an anonymous "&gt;&gt;" bound to references to a synthetic
  "vL_P" placeholder.

  A const "&gt;&gt;" handle (`a &gt;&gt; b!`, R-3.10.12) is a third case. Its
  value is dataized once and cached in a single binding, so a handle referenced
  more than once must stay one shared object rather than be inlined per use
  (which would mint an independent const at each site and drop the shared name,
  #5828). Such a handle keeps its "@local" marker here — the parser leaves it on
  the wrapped value inside the "const-to-dataized" `.as-bytes`/`Φ.dataized`
  shell — so the handle survives to "to-eo-tree" and prints as `a &gt;&gt; b!`.
  Unlike a void, its cactus "@name" is NOT promoted and its references are NOT
  rewritten: the binding stays obfuscated so "inline-cactoos" leaves it whole
  and "merge-monikers" folds it onto its first reference as a moniker, the other
  references reading back as the bare handle. A single-use const still inlines
  to `a!` (#5821).

  A pipe-application handle (`| args &gt;&gt; name`, §3.14 / #6015) is a fourth
  case. A pipe object carries the "@pipe" marker so that both later passes leave
  it standing — "inline-cactoos" never inlines it and "merge-monikers" never
  folds it or rewrites its references. Its readable handle therefore lives or
  dies entirely in this sheet: like a void, its "@local" marker is KEPT (at any
  use count) and its references are rewritten back to the handle, so it prints
  as `| args &gt;&gt; name` with references reading `name`. Unlike a void, its
  cactus "@name" is NOT promoted — the pipe reads its "&gt;&gt; name" from
  "@local" — so it stays a bare-named "@pipe" object all the way to
  "to-eo-tree".
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="auto" select="concat('a', $eo:cactoos)"/>
  <!--
  A reference resolves to its own auto-name: given a base such as
  "ξ.ρ.a🌵4-2", everything up to the cactus prefix is stripped, so the
  resolved name is the trailing "a🌵4-2". Mirrors "inline-cactoos".
  -->
  <xsl:function name="eo:resolved-name" as="xs:string">
    <xsl:param name="base" as="xs:string"/>
    <xsl:sequence select="substring-after($base, substring-before($base, $auto))"/>
  </xsl:function>
  <!--
  Whether the auto-named abstract "$target" transitively references its
  own name "$name", i.e. its subtree holds a reference that resolves back
  to "$name". Such an abstract is recursive and is never inlined by
  "inline-cactoos" (#5677), so its handle must be restored here.
  -->
  <xsl:function name="eo:recursive" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string?"/>
    <xsl:sequence select="exists($target//o[contains(@base, concat('.', $auto)) and eo:resolved-name(@base) = $name])"/>
  </xsl:function>
  <!--
  The references in the binding's owner that resolve to the auto-name "$name"
  (references inside the binding's own subtree excluded). Mirrors
  "inline-cactoos".
  -->
  <xsl:function name="eo:references" as="element()*">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string?"/>
    <xsl:sequence select="$target/..//o[contains(@base, concat('.', $auto)) and (eo:resolved-name(@base) = $name or starts-with(eo:resolved-name(@base), concat($name, '.'))) and not(ancestor-or-self::o[. is $target])]"/>
  </xsl:function>
  <!--
  Whether more than one reference in the binding's owner resolves to the
  auto-name "$name". A multi-referenced non-const abstract formation
  ("[] &gt;&gt; name", #5876) is never inlined by "inline-cactoos" (folding
  the whole formation into every use drops its shared handle name), so like a
  multi-referenced const handle its "@local" marker is kept here (below) and
  "merge-monikers" folds the surviving binding onto its first reference. A
  based handle ("a.b &gt;&gt; name", R-3.10.12) reached from more than one
  site is kept for the same reason (#5944): "inline-cactoos" never folds a
  reference spelled as a method dispatch ("name.seg"), nor any reference to a
  shared application, since each fold would build the application anew and
  print one object twice (#5956). The binding therefore outlives that sheet and
  only the hosting reference is merged into it — the others read the handle by
  name and would otherwise print a synthetic "vL_P".
  -->
  <xsl:function name="eo:multi-referenced" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string?"/>
    <xsl:sequence select="count(eo:references($target, $name)) &gt; 1"/>
  </xsl:function>
  <!--
  Whether no reference in the binding's owner resolves to the auto-name
  "$name". "inline-cactoos" keeps such a binding where it stands, having no use
  site to fold it into (#5914), so it reaches "to-eo-tree" and must carry a
  readable handle rather than print as an anonymous "&gt;&gt;".
  -->
  <xsl:function name="eo:unreferenced" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string?"/>
    <xsl:sequence select="empty(eo:references($target, $name))"/>
  </xsl:function>
  <!--
  Whether the auto-name "$name" is reached only from nested formation scopes: it
  is referenced (so not the "eo:unreferenced" case) yet no reference sits in the
  binding's own owner scope, every one climbing out of a formation nested inside
  it. A reference written in the owner's own body has that owner as its nearest
  formation ancestor; one inside a nested "&gt;&gt;" body has the nested formation
  instead, so it is excluded. Neither later pass folds such a reference:
  "inline-cactoos" computes an inline target straight off the base, so a nested
  method dispatch ("ξ.ρ.&lt;name&gt;.&lt;seg&gt;") resolves to
  "&lt;name&gt;.&lt;seg&gt;" and finds no target, and "merge-monikers" only hosts
  a reference whose nearest formation ancestor is the binding's own owner. The
  binding therefore reaches "to-eo-tree" and its "@local" is kept here (below), so
  "merge-monikers" rewrites the surviving reference back to the readable handle
  ("eo:kept-local-ref") — exactly as the multi-referenced spelling of the same
  shape is — rather than strand it on a synthetic "vL_P" placeholder (#5995).
  -->
  <xsl:function name="eo:nested-referenced" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string?"/>
    <xsl:sequence select="exists(eo:references($target, $name)) and empty(eo:references($target, $name)[ancestor::o[eo:abstract(.)][1] is $target/..])"/>
  </xsl:function>
  <!--
  Whether the auto-named abstract formation "$target" is applied by a reference
  that stands as a dispatch receiver rather than a following sibling. Such a
  reference resolves to "$name", carries its own argument children or a
  result-binding "@name" (so a fresh bare-reference inline would drop them,
  #5834), yet does not sit as a following sibling of the formation, the one
  shape a "| args &gt; name" pipe (#5834) or its adjacent relocation (#5840)
  already covers. A dispatch receiver "(bar 55).a" is exactly this: its
  "bar 55" receiver is buried as the "ρ" of the ".a" node. "inline-cactoos"
  relocates a copy of the formation into the receiver slot and turns the "ρ"
  into a "| 55" pipe (#5844); unlike the sibling pipe, that relocated
  predecessor sits inside the dispatch block, so its readable "&gt;&gt; name"
  handle is kept here — its "@local" marker survives, though (unlike a void)
  its cactus "@name" is left obfuscated so "inline-cactoos" still recognises
  the reference and pipes it. References inside the formation itself are
  excluded, so a self-referential (recursive) helper is not mistaken for an
  external use.
  -->
  <xsl:function name="eo:applied-receiver" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string?"/>
    <xsl:sequence select="eo:abstract($target) and exists($target/..//o[contains(@base, concat('.', $auto)) and eo:resolved-name(@base) = $name and (o or @name) and not(ancestor-or-self::o[. is $target]) and not(preceding-sibling::o[. is $target])])"/>
  </xsl:function>
  <!--
  Whether the based "&gt;&gt; name" handle "$target" is itself an application
  carrying its own argument children (`a.plus 1 &gt;&gt; b`) and is reached by a
  reference that applies it further (`b 42`). Such a target has no inline
  spelling as the head of another application — folding it would hand the inner
  application a second argument rather than apply its result — so "inline-cactoos"
  leaves the binding standing (see its "eo:reapplied") rather than folding it.
  The binding therefore reaches "to-eo-tree" and must keep its readable "@local"
  handle: "merge-monikers" rewrites the surviving reference back to it
  ("eo:kept-local-ref"), so it reads as `b 42` rather than a synthetic "vL_P"
  placeholder (#5952). A const handle (`@const`) and the dataized-const shell
  (`.as-bytes`) are excluded — they carry their name through a bare inline
  already (#5828) — and an abstract formation, handled by "eo:applied-receiver".
  -->
  <xsl:function name="eo:reapplied" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string?"/>
    <xsl:sequence select="not(eo:abstract($target)) and not(exists($target/@const)) and not($target/@base = '.as-bytes') and exists($target/o) and exists($target/..//o[contains(@base, concat('.', $auto)) and eo:resolved-name(@base) = $name and o and not(ancestor-or-self::o[. is $target])])"/>
  </xsl:function>
  <!--
  Whether "$wrapper" is a dataized-const file-local handle (`a &gt;&gt; b!`,
  R-3.10.12) that is referenced more than once. "const-to-dataized" wraps such
  a const in a `.as-bytes` over `Φ.dataized` node carrying the obfuscated
  cactus @name, with the readable handle kept as "@local" on the wrapped value.
  A const is dataized once and cached in that single binding, so every
  reference shares one const object; inlining it per use (as "inline-cactoos"
  does for a single-use const, #5821, or a referentially-transparent non-const
  handle, #5810) would mint an independent const object at each site and drop
  the shared name. Its "@local" marker is therefore kept here so the surviving
  binding still prints its readable `&gt;&gt; b` handle; the binding itself
  stays cactus-named for "merge-monikers" to fold onto its first reference
  (#5828). A reference reaches the handle either bare (`ξ.b`) or through a
  method dispatch (`ξ.b.seg`, e.g. `b.gte 1`, #5883), so both spellings are
  counted — exactly as "eo:multi-referenced" does; counting only the bare shape
  would leave a handle whose sibling reference is a dispatch looking
  single-use, dropping "@local" and stranding that dispatch on a synthetic
  "vL_P" placeholder.
  -->
  <xsl:function name="eo:const-handle" as="xs:boolean">
    <xsl:param name="wrapper" as="element()*"/>
    <xsl:variable name="value" select="$wrapper/o[@base='Φ.dataized']/o[1]"/>
    <xsl:sequence select="exists($wrapper) and $wrapper/@base='.as-bytes' and exists($wrapper/@name) and exists($value/@local) and count($wrapper/..//o[contains(@base, concat('.', $auto)) and (eo:resolved-name(@base) = $wrapper/@name or starts-with(eo:resolved-name(@base), concat($wrapper/@name, '.'))) and not(ancestor-or-self::o[. is $wrapper])]) &gt; 1"/>
  </xsl:function>
  <xsl:key name="void-handle" match="o[@local and (@base=$eo:empty or eo:recursive(., @name) or @pipe)]" use="@name"/>
  <!--
  References: rewrite each cactus segment that names a handled void, a
  recursive formation, or a pipe-application handle (`| args &gt;&gt; name`,
  §3.14 / #6015) back into the readable handle. A pipe object carries the
  "@pipe" marker, so neither "inline-cactoos" nor "merge-monikers" ever folds it
  or rewrites its references (both skip "@pipe" bindings); its references are
  therefore restored here, exactly as a void's are. A formation applied as a
  dispatch receiver (#5844) is deliberately excluded: its cactus name must
  survive so "inline-cactoos" still recognises the reference and pipes it.
  -->
  <xsl:template match="@base">
    <xsl:attribute name="base" select="string-join(for $seg in tokenize(., '\.') return (if (key('void-handle', $seg)) then key('void-handle', $seg)[1]/@local else $seg), '.')"/>
  </xsl:template>
  <!--
  Handled declaration (void or recursive formation): promote the handle
  to the visible name. A formation applied as a dispatch receiver (#5844) and a
  pipe-application handle (#6015) are not promoted — only their "@local" marker
  is kept (below) — so their cactus name survives: the dispatch receiver for
  "inline-cactoos" to pipe against, the pipe handle to read as a `|` line whose
  "&gt;&gt; name" comes from "@local" rather than a promoted "@name".
  -->
  <xsl:template match="o[@local and (@base=$eo:empty or eo:recursive(., @name))]/@name">
    <xsl:attribute name="name" select="../@local"/>
  </xsl:template>
  <!--
  Keep the marker on voids, on recursive formations, on a pipe-application
  handle (`| args &gt;&gt; name`, §3.14 / #6015) — never inlined or merged (it
  carries "@pipe", which both later passes skip), so its readable handle lives
  or dies here and must be kept at every use count — on a formation applied as
  a dispatch receiver (#5844) — so its relocated pipe predecessor prints its
  readable "&gt;&gt; name" handle — on a multi-referenced binding of any shape
  (an abstract formation, #5876, or a based handle, #5944) — kept whole by
  "inline-cactoos" and hosted by "merge-monikers" — on an
  unreferenced binding of any shape (#5914) — kept where it stands by
  "inline-cactoos", having no use site to fold into — on a binding reached only
  from nested formation scopes (see "eo:nested-referenced") — kept whole by
  "inline-cactoos" (a nested method dispatch is never folded) and hosted by
  "merge-monikers" onto no reference in its own scope, so every reference reads
  the handle by name (#5995) — on a based application
  handle reached by a further application (see "eo:reapplied") — left standing
  by "inline-cactoos", having no inline spelling as the head of another
  application (#5952) — and on the value of a
  multi-referenced dataized-const handle (see
  "eo:const-handle") so "to-eo-tree" restores the readable "&gt;&gt; name"
  handle; drop it on the other non-void formations, whose handle is inlined
  away by "inline-cactoos".
  -->
  <xsl:template match="o[not(@base=$eo:empty) and not(@pipe) and not(eo:recursive(., @name)) and not(eo:applied-receiver(., @name)) and not(eo:multi-referenced(., @name)) and not(eo:unreferenced(., @name)) and not(eo:nested-referenced(., @name)) and not(eo:reapplied(., @name)) and not(eo:const-handle(parent::o/parent::o))]/@local"/>
  <!--
  When a recursive "&gt;&gt; name" handle is restored, its cactus name is
  promoted to the visible "@name" and every reference is rewritten from the
  cactus name to the handle. That strips the cactus name before
  "inline-cactoos" runs, so its #5834 pipe-continuation logic
  (`eo:piped`) — which only matches cactus-named references — never fires
  and the applied sibling reference stays expanded (`bar &gt; x`) instead of
  folding to the compact `| &gt; x`. Mirror `eo:piped` here: tag the handle's
  immediately-following applied sibling reference — one carrying arguments or
  a name — with `pipe=""`. "to-eo-tree" renders "@pipe" when the base equals
  the preceding sibling's name, so tagging alone emits the "|". A reference
  that already carries "@pipe" (an already-piped handle round-tripping) is
  left as is.
  -->
  <xsl:template match="o[contains(@base, concat('.', $auto)) and (o or @name) and preceding-sibling::o[1][@local and eo:recursive(., @name)] and eo:resolved-name(@base) = preceding-sibling::o[1]/@name]">
    <xsl:copy>
      <xsl:if test="not(@pipe)">
        <xsl:attribute name="pipe"/>
      </xsl:if>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <!--
  The mirror case (#5848): the applied reference sits ABOVE the recursive
  handle, not below it. A "| args &gt; name" pipe binds its immediately-preceding
  sibling, so a reference standing before the handle cannot pipe against it
  where it is; the #5837 tag-in-place path above only matches a reference whose
  preceding sibling is the handle, so it never fires and the reference stays
  expanded ("bar &gt; @"). Relocate it instead: suppress the reference at its
  origin and re-emit it just under the handle tagged "@pipe", the same
  relocate-and-pipe "inline-cactoos" performs for a separated non-recursive
  handle (#5840) — done here because the recursive handle is never inlined and
  its cactus name is stripped before "inline-cactoos" runs. The reference need
  not be the handle's immediate sibling; any applied reference resolving to the
  handle among its preceding siblings folds, mirroring #5840's separated case.

  Suppress the reference at its origin. Match any applied reference (one
  carrying arguments or a name) with a recursive handle among its following
  siblings, then keep only those that resolve to it — a reference resolving to a
  different binding still prints in place.
  -->
  <xsl:template match="o[contains(@base, concat('.', $auto)) and (o or @name) and following-sibling::o[@local and eo:recursive(., @name)]]">
    <xsl:if test="not(following-sibling::o[@local and eo:recursive(., @name) and @name = eo:resolved-name(current()/@base)])">
      <xsl:copy>
        <xsl:apply-templates select="node()|@*"/>
      </xsl:copy>
    </xsl:if>
  </xsl:template>
  <!--
  Re-emit the suppressed reference below the handle. Match the recursive handle,
  copy it, then for each applied reference among its preceding siblings that
  resolves to it emit a "@pipe"-tagged copy; "to-eo-tree" renders the "|"
  because the reference's restored base now equals the preceding sibling's name.
  A reference already carrying "@pipe" is left as is. A handle with no such
  preceding reference (the #5837 reference-below shape) just copies through.
  -->
  <xsl:template match="o[@local and eo:recursive(., @name)]">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
    <xsl:for-each select="preceding-sibling::o[contains(@base, concat('.', $auto)) and (o or @name) and eo:resolved-name(@base) = current()/@name]">
      <xsl:copy>
        <xsl:if test="not(@pipe)">
          <xsl:attribute name="pipe"/>
        </xsl:if>
        <xsl:apply-templates select="node()|@*"/>
      </xsl:copy>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
