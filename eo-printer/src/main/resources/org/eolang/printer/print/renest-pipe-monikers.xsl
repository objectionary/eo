<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs" id="renest-pipe-monikers" version="2.0">
  <!--
  Re-nests a pipe target formation back above its pipe use-site, restoring
  the compact `| args` continuation line (§3.14). A `| args` line applies
  its same-indent predecessor; the parser rewrites it into an application
  whose `@base` points at that predecessor's name and keeps a `@pipe`
  marker (#5684). When the predecessor formation sits inside an argument
  block (its parent is itself an application), `wrap-applications` /
  `vars-float-up` hoist that formation up to the enclosing abstract object
  and drop it from the argument slot (#5526), leaving the pipe application
  as the block's sole remaining member. The predecessor has then floated
  away from the pipe, so `to-eo-tree`'s adjacency guard fails and the line
  prints as an ordinary application instead of a `|` — the compact author
  spelling is lost on the first format.

  This pass mirrors `merge-monikers`: for a formation attribute whose only
  reference is a single `@pipe` application that no longer sits directly
  after it, it copies the formation back in front of that pipe use-site and
  drops the floated original. The pipe then reads its predecessor from the
  immediately preceding sibling, so the adjacency guard at
  `to-eo-tree.xsl` fires and the `|` head is emitted. Both spellings parse
  to identical XMIR, so re-nesting is safe.

  Only a formation with exactly one reference is re-nested, and that
  reference must be the pipe use itself. This is the same single-host
  condition `merge-monikers` uses (#5706): a second reference would fall
  out of scope once the formation moved into the pipe's argument block, and
  the choice of a use-site would not survive the print/parse round trip.
  A formation already adjacent to its pipe use (never floated, or floated
  but still landing next to it) is left untouched.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  The leading name a reference `@base` resolves to: the first segment after
  an optional `ξ.` root. `ξ.hello` and a bare `hello` both resolve to
  `hello`; `ξ.hello.tail` or `ξ.ρ.hello` resolve to their own first
  segment and so never collide with the target name.
  -->
  <xsl:function name="eo:ref-head" as="xs:string">
    <xsl:param name="base" as="xs:string"/>
    <xsl:variable name="tail" select="if (starts-with($base, concat($eo:xi, '.'))) then substring-after($base, concat($eo:xi, '.')) else $base"/>
    <xsl:sequence select="tokenize($tail, '\.')[1]"/>
  </xsl:function>
  <!--
  Whether `$attr` is a formation attribute eligible to host a re-nested
  pipe: named, abstract (a formation, so it is what a pipe predecessor
  becomes after floating up), owned by an abstract object, and neither `φ`
  nor a test.
  -->
  <xsl:function name="eo:pipe-binding" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:sequence select="exists($attr/@name) and eo:abstract($attr) and eo:abstract($attr/..) and $attr/@name != $eo:phi and not(eo:test-attr($attr))"/>
  </xsl:function>
  <!--
  The references to the binding `$attr`'s name reachable through no
  intervening formation (so the name still binds to the same formation) and
  not sitting inside the binding itself (a recursive self-reference).
  -->
  <xsl:function name="eo:pipe-refs" as="element()*">
    <xsl:param name="attr" as="element()"/>
    <xsl:variable name="owner" select="$attr/.."/>
    <xsl:sequence select="$owner//o[exists(@base) and eo:ref-head(@base) = $attr/@name and (ancestor::o[eo:abstract(.)][1] is $owner) and not(ancestor::o[. is $attr])]"/>
  </xsl:function>
  <!--
  Whether `$attr` is a pipe binding that has floated away from its use-site:
  its single reference is a `@pipe` application that is not already its
  immediate following sibling.
  -->
  <xsl:function name="eo:pipe-floated" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:variable name="refs" select="eo:pipe-refs($attr)"/>
    <xsl:sequence select="eo:pipe-binding($attr) and count($refs) = 1 and exists($refs[1]/@pipe) and not($refs[1]/preceding-sibling::o[1] is $attr)"/>
  </xsl:function>
  <!--
  The floated binding that a reference `$ref` should be re-nested in front
  of, or the empty sequence when `$ref` hosts none.
  -->
  <xsl:function name="eo:pipe-host" as="element()*">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="owner" select="$ref/ancestor::o[eo:abstract(.)][1]"/>
    <xsl:sequence select="$owner/o[eo:pipe-floated(.) and eo:pipe-refs(.)[1] is $ref][1]"/>
  </xsl:function>
  <!--
  Emit the re-nested binding immediately before its pipe use-site, then the
  pipe application itself.
  -->
  <xsl:template match="o[exists(eo:pipe-host(.))]" priority="1">
    <xsl:variable name="binding" select="eo:pipe-host(.)"/>
    <xsl:element name="o">
      <xsl:apply-templates select="$binding/@*"/>
      <xsl:apply-templates select="$binding/node()"/>
    </xsl:element>
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  <!--
  Drop the floated binding once it has been re-nested onto its pipe.
  -->
  <xsl:template match="o[eo:pipe-floated(.)]" priority="1"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
