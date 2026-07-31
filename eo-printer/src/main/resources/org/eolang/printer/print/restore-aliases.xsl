<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs" id="restore-aliases" version="2.0">
  <!--
  The printing inverse of the parser's "resolve-aliases" (#6184). The parser
  turns a bare aliased reference into the fully-qualified name its "+alias"
  declares (`appellation` under `+alias eo3.pardon.appellation` becomes
  `Φ.eo3.pardon.appellation`) and then discards the alias's role; nothing on
  the printing side put it back, so every reference read verbose and the
  surviving "+alias" meta was left dead (which "unused-alias" flags). This pass
  restores the short spelling, keyed on how often each aliased name is
  referenced: referenced more than once, its "+alias" survives and every
  reference is shortened back to the alias name; referenced once or not at all,
  the "+alias" is dropped and the (single) reference is left fully qualified,
  which reads back the same on its own.
  Only names a "+alias" declares are touched, and only when the short name
  cannot be misread on reparse. The parser flattens a method-dispatch chain
  onto a global (`os.is-windows.if`) into the very same "Φ.os.is-windows.if"
  shape a package import carries, so an undeclared qualified name is left alone
  rather than mistaken for an import. An alias whose short name is also an
  object, void or handle name in the file (`+alias bytes.hash` beside a local
  `hash`), or a bare global, is left untouched too: shortening would rebind the
  reference to that local or global instead.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- The alias metas the program declares, captured so the functions can reach them without a context. -->
  <xsl:variable name="aliases" select="/object/metas/meta[head = 'alias']"/>
  <!-- Every name already bound in the file, so an alias short name that clashes with one is left alone. -->
  <xsl:variable name="bound" select="distinct-values((//o/@name, //o/@local))"/>
  <!-- Every reference string, counted with multiplicity, across the four spots resolve-aliases rewrote. -->
  <xsl:variable name="refs" as="xs:string*">
    <xsl:sequence select="//o/@base/string()"/>
    <xsl:sequence select="//o/@atom/string()"/>
    <xsl:sequence select="for $a in //o/@args, $t in tokenize($a, ' ')[. != ''] return $t"/>
    <xsl:sequence select="for $t in //o/@type return replace($t, '\?$', '')"/>
  </xsl:variable>
  <!-- Whether an alias's short name reads back unambiguously: no bound name and no bare global claims it. -->
  <xsl:function name="eo:free" as="xs:boolean">
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="not($name = $bound) and not($refs = concat($eo:program, '.', $name))"/>
  </xsl:function>
  <!-- The fully-qualified names worth restoring: a free-named alias referenced more than once. -->
  <xsl:variable name="kept" as="xs:string*" select="for $m in $aliases return if (eo:free($m/part[1]) and count($refs[. = $m/part[last()]]) gt 1) then string($m/part[last()]) else ()"/>
  <!-- The short name a kept fully-qualified name restores to, or the value itself when it is not kept. -->
  <xsl:function name="eo:shorten" as="xs:string">
    <xsl:param name="value" as="xs:string"/>
    <xsl:sequence select="(for $m in $aliases[part[last()] = $value and part[last()] = $kept] return string($m/part[1]), $value)[1]"/>
  </xsl:function>
  <xsl:template match="o/@base">
    <xsl:attribute name="base" select="eo:shorten(.)"/>
  </xsl:template>
  <xsl:template match="o/@atom">
    <xsl:attribute name="atom" select="eo:shorten(.)"/>
  </xsl:template>
  <xsl:template match="o/@args">
    <xsl:attribute name="args" select="string-join(for $t in tokenize(., ' ')[. != ''] return eo:shorten($t), ' ')"/>
  </xsl:template>
  <xsl:template match="o/@type">
    <xsl:variable name="opt" select="ends-with(., '?')"/>
    <xsl:attribute name="type" select="concat(eo:shorten(replace(., '\?$', '')), if ($opt) then '?' else '')"/>
  </xsl:template>
  <!-- Drop a dead or single-use alias whose short name was free to restore; a clashing alias is left in place. -->
  <xsl:template match="metas/meta[head = 'alias' and eo:free(part[1]) and not(part[last()] = $kept)]"/>
  <!-- Drop the whole "metas" block when nothing survives, so a file of only dead aliases reads like one with no metas. -->
  <xsl:template match="metas[every $m in meta satisfies ($m/head = 'alias' and eo:free($m/part[1]) and not($m/part[last()] = $kept))]"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
