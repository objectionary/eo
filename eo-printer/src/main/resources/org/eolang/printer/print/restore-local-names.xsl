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
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="exists($target//o[contains(@base, concat('.', $auto)) and eo:resolved-name(@base) = $name])"/>
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
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="eo:abstract($target) and exists($target/..//o[contains(@base, concat('.', $auto)) and eo:resolved-name(@base) = $name and (o or @name) and not(ancestor-or-self::o[. is $target]) and not(preceding-sibling::o[. is $target])])"/>
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
  (#5828).
  -->
  <xsl:function name="eo:const-handle" as="xs:boolean">
    <xsl:param name="wrapper" as="element()*"/>
    <xsl:variable name="value" select="$wrapper/o[@base='Φ.dataized']/o[1]"/>
    <xsl:sequence select="exists($wrapper) and $wrapper/@base='.as-bytes' and exists($wrapper/@name) and exists($value/@local) and count($wrapper/..//o[contains(@base, concat('.', $auto)) and eo:resolved-name(@base) = $wrapper/@name and not(ancestor-or-self::o[. is $wrapper])]) &gt; 1"/>
  </xsl:function>
  <xsl:key name="void-handle" match="o[@local and (@base=$eo:empty or eo:recursive(., @name))]" use="@name"/>
  <!--
  References: rewrite each cactus segment that names a handled void or a
  recursive formation back into the readable handle. A formation applied as a
  dispatch receiver (#5844) is deliberately excluded: its cactus name must
  survive so "inline-cactoos" still recognises the reference and pipes it.
  -->
  <xsl:template match="@base">
    <xsl:attribute name="base" select="string-join(for $seg in tokenize(., '\.') return (if (key('void-handle', $seg)) then key('void-handle', $seg)[1]/@local else $seg), '.')"/>
  </xsl:template>
  <!--
  Handled declaration (void or recursive formation): promote the handle
  to the visible name. A formation applied as a dispatch receiver (#5844) is
  not promoted — only its "@local" marker is kept (below) — so its cactus name
  survives for "inline-cactoos" to pipe against.
  -->
  <xsl:template match="o[@local and (@base=$eo:empty or eo:recursive(., @name))]/@name">
    <xsl:attribute name="name" select="../@local"/>
  </xsl:template>
  <!--
  Keep the marker on voids, on recursive formations, on a formation applied as
  a dispatch receiver (#5844) — so its relocated pipe predecessor prints its
  readable "&gt;&gt; name" handle — and on the value of a multi-referenced
  dataized-const handle (see "eo:const-handle") so "to-eo-tree" restores the
  readable "&gt;&gt; name" handle; drop it on the other non-void formations,
  whose handle is inlined away by "inline-cactoos".
  -->
  <xsl:template match="o[not(@base=$eo:empty) and not(eo:recursive(., @name)) and not(eo:applied-receiver(., @name)) and not(eo:const-handle(parent::o/parent::o))]/@local"/>
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
  The mirror case (#5848): the applied sibling reference sits just ABOVE the
  recursive handle, not below it. A "| args &gt; name" pipe binds its
  immediately-preceding sibling, so a reference standing before the handle
  cannot pipe against it where it is; the #5837 tag-in-place path above only
  matches a reference whose preceding sibling is the handle, so it never fires
  and the reference stays expanded ("bar &gt; @"). Relocate it instead: drop
  the reference from its own position (below) and re-emit it just under the
  handle, tagged "@pipe", the same relocation "inline-cactoos" performs for a
  separated non-recursive handle (#5840) — done here because the recursive
  handle is never inlined and its cactus name is stripped before
  "inline-cactoos" runs. The reference is suppressed at its origin:
  -->
  <xsl:template match="o[contains(@base, concat('.', $auto)) and (o or @name) and following-sibling::o[1][@local and eo:recursive(., @name)] and eo:resolved-name(@base) = following-sibling::o[1]/@name]"/>
  <!--
  and re-emitted below the handle. Match the recursive handle whose
  immediately-preceding sibling is such an applied reference, copy the handle,
  then emit that reference tagged "@pipe"; "to-eo-tree" renders the "|" because
  the reference's restored base now equals the preceding sibling's name. A
  reference already carrying "@pipe" is left as is.
  -->
  <xsl:template match="o[@local and eo:recursive(., @name) and preceding-sibling::o[1][contains(@base, concat('.', $auto)) and (o or @name)]]">
    <xsl:variable name="ref" select="preceding-sibling::o[1]"/>
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
    <xsl:if test="eo:resolved-name($ref/@base) = @name">
      <xsl:for-each select="$ref">
        <xsl:copy>
          <xsl:if test="not(@pipe)">
            <xsl:attribute name="pipe"/>
          </xsl:if>
          <xsl:apply-templates select="node()|@*"/>
        </xsl:copy>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
