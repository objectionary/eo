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
  #5828). Such a handle is likewise treated as a handled void — its handle,
  which the parser leaves on the wrapped value inside the "const-to-dataized"
  `.as-bytes`/`Φ.dataized` shell, is promoted to the wrapper's visible name, its
  references are rewritten back to the handle, and the marker is kept — so it
  survives the later passes as a single standalone `a &gt;&gt; b!` binding. A
  single-use const still inlines to `a!` (#5821).
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
  Whether "$wrapper" is a dataized-const file-local handle (`a &gt;&gt; b!`,
  R-3.10.12) that is referenced more than once. "const-to-dataized" wraps such
  a const in a `.as-bytes` over `Φ.dataized` node carrying the obfuscated
  cactus @name, with the readable handle kept as "@local" on the wrapped value.
  A const is dataized once and cached in that single binding, so every
  reference shares one const object; inlining it per use (as "inline-cactoos"
  does for a single-use const, #5821, or a referentially-transparent non-const
  handle, #5810) would mint an independent const object at each site and drop
  the shared name. A multi-referenced one is therefore restored here like a
  handled void: its handle is promoted to the visible name and its references
  are rewritten back to the handle, so it stays a single standalone
  `a &gt;&gt; b!` binding that the later passes leave untouched (#5828).
  -->
  <xsl:function name="eo:const-handle" as="xs:boolean">
    <xsl:param name="wrapper" as="element()*"/>
    <xsl:variable name="value" select="$wrapper/o[@base='Φ.dataized']/o[1]"/>
    <xsl:sequence select="exists($wrapper) and $wrapper/@base='.as-bytes' and exists($wrapper/@name) and exists($value/@local) and count($wrapper/..//o[contains(@base, concat('.', $auto)) and eo:resolved-name(@base) = $wrapper/@name and not(ancestor-or-self::o[. is $wrapper])]) &gt; 1"/>
  </xsl:function>
  <xsl:key name="void-handle" match="o[@local and (@base=$eo:empty or eo:recursive(., @name))]" use="@name"/>
  <xsl:key name="const-handle" match="o[eo:const-handle(.)]" use="@name"/>
  <!--
  References: rewrite each cactus segment that names a handled void, a
  recursive formation, or a multi-referenced dataized-const handle back into
  the readable handle. A void or recursive formation carries "@local" itself; a
  const wrapper keeps the handle on its wrapped value.
  -->
  <xsl:template match="@base">
    <xsl:attribute name="base" select="string-join(for $seg in tokenize(., '\.') return (if (key('void-handle', $seg)) then string(key('void-handle', $seg)[1]/@local) else if (key('const-handle', $seg)) then string(key('const-handle', $seg)[1]/o[@base='Φ.dataized']/o[1]/@local) else $seg), '.')"/>
  </xsl:template>
  <!--
  Handled declaration (void or recursive formation): promote the handle
  to the visible name.
  -->
  <xsl:template match="o[@local and (@base=$eo:empty or eo:recursive(., @name))]/@name">
    <xsl:attribute name="name" select="../@local"/>
  </xsl:template>
  <!--
  Multi-referenced dataized-const handle: promote its handle (kept on the
  wrapped value) to the wrapper's visible name, so "dataized-to-const" rebuilds
  a readable `a &gt;&gt; b!` binding rather than a cactus-named one that
  "merge-monikers" would try to fold onto a use site.
  -->
  <xsl:template match="o[eo:const-handle(.)]/@name">
    <xsl:attribute name="name" select="../o[@base='Φ.dataized']/o[1]/@local"/>
  </xsl:template>
  <!--
  Keep the marker on voids, on recursive formations, and on the value of a
  multi-referenced dataized-const handle so "to-eo-tree" restores the readable
  "&gt;&gt; name" handle; drop it on the other non-void formations, whose
  handle is inlined away by "inline-cactoos".
  -->
  <xsl:template match="o[not(@base=$eo:empty) and not(eo:recursive(., @name)) and not(eo:const-handle(parent::o/parent::o))]/@local"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
