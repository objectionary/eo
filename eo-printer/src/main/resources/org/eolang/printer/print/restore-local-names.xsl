<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="restore-local-names" version="2.0">
  <!--
  Inverse of the parser's "resolve-local-names" pass, applied before
  printing (#5563). An object declared with a file-local handle ("&gt;&gt;
  name", §3.10 / §9.2) keeps a synthetic cactus @name plus a "@local='name'"
  marker; the parser resolves references to the cactus name, and here we put
  the readable handle back: the declaration's @name becomes its handle and
  every @base segment pointing at that cactus name is rewritten to the
  handle, so references read under the handle instead of a synthetic "vL_P"
  placeholder. The "@local" marker is KEPT (#5581) so "to-eo-tree" prints a
  "&gt;&gt; name" line rather than collapsing it into a "[name]" bracket.

  A handle is restored only when it must survive to the printed source:
  a void (never inlined), a self-referential (recursive) formation (kept by
  "inline-cactoos", #5677), or a handle with references that a single
  moniker inline cannot absorb — more than one reference, or a lone
  reference that is not hostable (a multi-segment dispatch such as
  "h.a.b"). A handle whose only reference is a hostable one (bare, or a
  single-segment dispatch) is left as its anonymous cactus name instead:
  "merge-monikers" inlines it at that sole reference and "to-eo-tree" prints
  it as an anonymous "&gt;&gt;", so an obfuscated single-use handle never
  leaks a synthetic name (#5782).
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
  Whether the auto-named abstract "$target" transitively references its own
  name "$name": its subtree holds a reference that resolves back to "$name".
  Such an abstract is recursive and is never inlined by "inline-cactoos"
  (#5677), so its handle must be restored here.
  -->
  <xsl:function name="eo:recursive" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="exists($target//o[contains(@base, concat('.', $auto)) and eo:resolved-name(@base) = $name])"/>
  </xsl:function>
  <!--
  Whether the reference "$ref" can host a moniker for cactus "$name", i.e.
  "merge-monikers" would inline the binding there: a bare "ξ.name" (with no
  argument that carries its own arguments) or a single-segment dispatch
  "ξ.name.seg". A named reference, a climb such as "ξ.ρ.name", or a
  multi-segment dispatch cannot host it. Mirrors "merge-monikers".
  -->
  <xsl:function name="eo:hostable-ref" as="xs:boolean">
    <xsl:param name="ref" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:variable name="tail" select="substring-after($ref/@base, concat($eo:xi, '.'))"/>
    <xsl:sequence select="not(exists($ref/@name)) and (($tail = $name and not($ref/o[o])) or (starts-with($tail, concat($name, '.')) and not(contains(substring-after($tail, concat($name, '.')), '.'))))"/>
  </xsl:function>
  <!--
  Whether the handle "$name" of "$target" is absorbed whole by a single
  moniker inline: it has exactly one reference outside its own subtree and
  that reference is hostable. Such a handle need not be restored, so it
  prints anonymously (#5782). More than one reference, or a lone
  non-hostable one, leaves it un-reducible and the handle is kept.
  -->
  <xsl:function name="eo:reducible" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:variable name="ext" select="root($target)//o[$name = tokenize(@base, '\.') and not(ancestor-or-self::o[. is $target])]"/>
    <xsl:sequence select="count($ext) = 1 and eo:hostable-ref($ext[1], $name)"/>
  </xsl:function>
  <!--
  Whether the handled declaration "$target" keeps its readable handle:
  a void, a recursive formation, or a handle no single inline can absorb.
  -->
  <xsl:function name="eo:restore" as="xs:boolean">
    <xsl:param name="target" as="element()"/>
    <xsl:sequence select="exists($target/@local) and ($target/@base=$eo:empty or eo:recursive($target, $target/@name) or not(eo:reducible($target, $target/@name)))"/>
  </xsl:function>
  <xsl:key name="handle" match="o[@local]" use="@name"/>
  <!--
  References: rewrite each cactus segment that names a restored handle back
  into its readable handle; a reducible (anonymous) handle's segment stays.
  -->
  <xsl:template match="@base">
    <xsl:attribute name="base" select="string-join(for $seg in tokenize(., '\.') return (if (key('handle', $seg) and eo:restore(key('handle', $seg)[1])) then key('handle', $seg)[1]/@local else $seg), '.')"/>
  </xsl:template>
  <!--
  Restored declaration: promote the handle to the visible name.
  -->
  <xsl:template match="o[@local and eo:restore(.)]/@name">
    <xsl:attribute name="name" select="../@local"/>
  </xsl:template>
  <!--
  Reducible handle: drop the marker, so it inlines under its anonymous
  cactus name instead of leaking a synthetic handle.
  -->
  <xsl:template match="o[@local and not(eo:restore(.))]/@local"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
