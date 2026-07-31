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
  restores the short spelling and rebuilds the alias metas to match, keyed on
  how often each fully-qualified name is referenced:

    - referenced more than once -> a "+alias a.b.c" is emitted and every
      reference is shortened to its bare last segment ("appellation");
    - referenced exactly once -> the reference is left fully qualified
      ("eo3.pardon.appellation") and no alias is emitted for it;
    - referenced zero times (an alias the source declared but never used, or
      one left dead by the single-reference case above) -> no alias is
      emitted, so the dead meta is dropped.

  Only a genuinely qualified name (Φ.<a>.<b>… with at least two name segments)
  can be aliased; a bare global (Φ.number) already prints at its shortest and
  a same-package self-reference (Φ.<package>.…) is left to "to-eo-tree", which
  prints it bare on its own. A short name is introduced only when it cannot be
  misread on reparse: it must not clash with any object or void name in the
  file, with a bare global (Φ.<name>), or with another qualified name that
  shortens to the very same segment.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- The document root, captured so name lookups survive a string context. -->
  <xsl:variable name="root" select="/"/>
  <!-- The current program's "+package" (empty when there is none). -->
  <xsl:variable name="package" select="string(/object/metas/meta[head='package']/part[1])"/>
  <!-- The "Φ.<package>." prefix a same-file self-reference carries, or the empty string. -->
  <xsl:variable name="self" select="if ($package = '') then '' else concat($eo:program, '.', $package, '.')"/>
  <!-- The last dotted segment of a name, its idiomatic alias short form. -->
  <xsl:function name="eo:segment" as="xs:string">
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="tokenize($name, '\.')[last()]"/>
  </xsl:function>
  <!--
  Whether a reference string is a qualified name eligible for aliasing: it is
  rooted at "Φ.", carries at least two name segments after that root, and is
  not a same-package self-reference (which "to-eo-tree" already prints bare).
  -->
  <xsl:function name="eo:qualified" as="xs:boolean">
    <xsl:param name="value" as="xs:string"/>
    <xsl:sequence select="starts-with($value, concat($eo:program, '.')) and contains(substring-after($value, concat($eo:program, '.')), '.') and not($self != '' and starts-with($value, $self))"/>
  </xsl:function>
  <!--
  Every reference string the program carries, counted with multiplicity: the
  "@base" of an object, the "@atom" of an atom's signature, each space-joined
  token of a void's "@args" callback types, and a void's own "@type" (with any
  trailing "?" maybe-⊥ marker stripped). These are exactly the four spots the
  parser's "resolve-aliases" rewrote, so counting them mirrors that inverse.
  -->
  <xsl:variable name="refs" as="xs:string*">
    <xsl:sequence select="//o/@base/string()"/>
    <xsl:sequence select="//o/@atom/string()"/>
    <xsl:sequence select="for $a in //o/@args, $t in tokenize($a, ' ')[. != ''] return $t"/>
    <xsl:sequence select="for $t in //o/@type return replace($t, '\?$', '')"/>
  </xsl:variable>
  <!-- The qualified references, and the distinct qualified names among them. -->
  <xsl:variable name="qualified" as="xs:string*" select="$refs[eo:qualified(.)]"/>
  <!-- The distinct qualified names referenced more than once. -->
  <xsl:variable name="repeated" as="xs:string*" select="for $d in distinct-values($qualified) return if (count($qualified[. = $d]) gt 1) then $d else ()"/>
  <!--
  The qualified names that become aliases: each referenced more than once and
  whose bare last segment cannot be misread on reparse — no object or void in
  the file already carries that name, no bare global "Φ.<segment>" is
  referenced, and no other aliasable name shortens to the same segment (which
  would make two aliases collide). A name failing any guard keeps its full
  spelling everywhere, which reads back unambiguously on its own.
  -->
  <xsl:variable name="aliased" as="xs:string*">
    <xsl:for-each select="$repeated">
      <xsl:variable name="fqn" select="."/>
      <xsl:variable name="short" select="eo:segment($fqn)"/>
      <xsl:if test="empty($root//o[@name = $short]) and not($refs = concat($eo:program, '.', $short)) and count($repeated[eo:segment(.) = $short]) = 1">
        <xsl:sequence select="$fqn"/>
      </xsl:if>
    </xsl:for-each>
  </xsl:variable>
  <!-- The aliased names in a stable order, so the emitted metas are deterministic. -->
  <xsl:variable name="ordered" as="xs:string*">
    <xsl:perform-sort select="$aliased">
      <xsl:sort select="."/>
    </xsl:perform-sort>
  </xsl:variable>
  <!-- Shorten a single reference string to its alias segment, or leave it as is. -->
  <xsl:function name="eo:shorten" as="xs:string">
    <xsl:param name="value" as="xs:string"/>
    <xsl:sequence select="if ($value = $aliased) then eo:segment($value) else $value"/>
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
  <!--
  Rebuild the alias metas from the usage counts above. Every non-alias meta is
  copied in its original position; the freshly generated alias metas take the
  slot the first source alias held (or, when the source declared none, the top
  of the block), and all source alias metas are dropped. When nothing is left,
  the "metas" element is omitted entirely, so a file whose only metas were dead
  aliases prints like one that never had any.
  -->
  <xsl:template match="metas">
    <xsl:variable name="generated" as="element()*">
      <xsl:for-each select="$ordered">
        <meta>
          <head>alias</head>
          <tail>
            <xsl:value-of select="concat(eo:segment(.), ' ', .)"/>
          </tail>
          <part>
            <xsl:value-of select="eo:segment(.)"/>
          </part>
          <part>
            <xsl:value-of select="."/>
          </part>
        </meta>
      </xsl:for-each>
    </xsl:variable>
    <xsl:if test="exists($generated) or exists(meta[head != 'alias'])">
      <metas>
        <xsl:if test="empty(meta[head = 'alias'])">
          <xsl:sequence select="$generated"/>
        </xsl:if>
        <xsl:for-each select="meta">
          <xsl:choose>
            <xsl:when test="head = 'alias'">
              <xsl:if test=". is (../meta[head = 'alias'])[1]">
                <xsl:sequence select="$generated"/>
              </xsl:if>
            </xsl:when>
            <xsl:otherwise>
              <xsl:copy-of select="."/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </metas>
    </xsl:if>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
