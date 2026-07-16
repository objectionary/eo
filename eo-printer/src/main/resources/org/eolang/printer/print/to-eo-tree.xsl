<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="to-eo-tree" version="2.0">
  <!--
  This one maps XMIR to an intermediate "line tree" that is later
  laid out into pretty EO source by the Pretty class (penalty-based
  formatting). Each object becomes a <line> element carrying its
  rendered head ("base" attribute) and suffix ("tail" attribute),
  with nested <line> children for its (non-void) attributes. The
  layout engine decides, per node, whether to render it horizontally
  (inline) or vertically (indented), picking the option with the
  lowest penalty.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:variable name="eol" select="'&#10;'"/>
  <xsl:output method="xml" encoding="UTF-8"/>
  <!-- Translate a dotted path to EO surface form: ρ -> ^, φ -> @. -->
  <xsl:function name="eo:translate-path" as="xs:string">
    <xsl:param name="path" as="xs:string"/>
    <xsl:sequence select="string-join(for $seg in tokenize($path, '\.') return (if ($seg = $eo:rho) then '^' else if ($seg = $eo:phi) then '@' else $seg), '.')"/>
  </xsl:function>
  <!-- Render a base as an EO head: drop implicit ξ./Φ. roots, render a
       leading dot as reversed dispatch, map ξ/ρ/φ/Φ and every segment. -->
  <xsl:function name="eo:surface" as="xs:string">
    <xsl:param name="base" as="xs:string"/>
    <xsl:sequence select="if (starts-with($base, '.')) then concat(eo:translate-path(substring($base, 2)), '.') else if (starts-with($base, concat($eo:program, '.'))) then eo:translate-path(substring-after($base, concat($eo:program, '.'))) else if (starts-with($base, concat($eo:xi, '.'))) then eo:translate-path(substring-after($base, concat($eo:xi, '.'))) else if ($base = $eo:xi) then '$' else if ($base = $eo:program) then 'Q' else eo:translate-path($base)"/>
  </xsl:function>
  <!-- First name segment of a program-rooted base (Φ.foo.bar -> foo). -->
  <xsl:function name="eo:root-name" as="xs:string">
    <xsl:param name="base" as="xs:string"/>
    <xsl:variable name="rest" select="substring-after($base, concat($eo:program, '.'))"/>
    <xsl:sequence select="if (contains($rest, '.')) then substring-before($rest, '.') else $rest"/>
  </xsl:function>
  <!-- Rewrite surviving cactus names (a🌵L-P) to a valid id (vL_P). -->
  <xsl:function name="eo:printable" as="xs:string">
    <xsl:param name="text" as="xs:string"/>
    <xsl:sequence select="replace($text, concat('a', $eo:cactoos, '(\d+)-(\d+)'), 'v$1_$2')"/>
  </xsl:function>
  <!-- PROGRAM -->
  <xsl:template match="object">
    <object>
      <eo>
        <preamble>
          <xsl:apply-templates select="comments"/>
          <xsl:apply-templates select="license"/>
          <xsl:apply-templates select="metas"/>
        </preamble>
        <xsl:apply-templates select="o[1]" mode="tree"/>
      </eo>
    </object>
  </xsl:template>
  <!-- TOP COMMENT BLOCK -->
  <xsl:template match="comments">
    <xsl:for-each select="comment">
      <xsl:for-each select="tokenize(., $eol)">
        <xsl:choose>
          <xsl:when test=". = ''">
            <xsl:text>#</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text># </xsl:text>
            <xsl:value-of select="."/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:value-of select="$eol"/>
      </xsl:for-each>
    </xsl:for-each>
    <xsl:if test="comment">
      <xsl:value-of select="$eol"/>
    </xsl:if>
  </xsl:template>
  <!-- LICENCE -->
  <xsl:template match="license">
    <xsl:for-each select="tokenize(., $eol)">
      <xsl:text># </xsl:text>
      <xsl:value-of select="."/>
      <xsl:value-of select="$eol"/>
    </xsl:for-each>
    <xsl:if test="text()">
      <xsl:value-of select="$eol"/>
    </xsl:if>
  </xsl:template>
  <!-- METAS -->
  <xsl:template match="metas">
    <xsl:apply-templates select="meta"/>
    <xsl:value-of select="$eol"/>
  </xsl:template>
  <!-- META -->
  <xsl:template match="meta">
    <xsl:text>+</xsl:text>
    <xsl:value-of select="head"/>
    <!-- An alias is stored as two parts: a local name and the fully
         qualified name (Φ.a.b.c). When the name is just the last dotted
         segment of the FQN it carries no information, so the idiomatic
         short form drops it and prints only the FQN with its Φ. root
         stripped (+alias a.b.c). A genuine rename, where the name
         differs from that last segment, keeps the two-argument form. -->
    <xsl:variable name="fqn" select="replace(part[last()], concat('^', $eo:program, '\.'), '')"/>
    <xsl:choose>
      <xsl:when test="head = 'alias' and count(part) = 2 and part[1] = tokenize($fqn, '\.')[last()]">
        <xsl:text> </xsl:text>
        <xsl:value-of select="$fqn"/>
      </xsl:when>
      <xsl:when test="not(empty(tail/text()))">
        <xsl:text> </xsl:text>
        <xsl:value-of select="replace(string(tail), $eo:program, 'Q')"/>
      </xsl:when>
    </xsl:choose>
    <xsl:value-of select="$eol"/>
  </xsl:template>
  <!-- OBJECT, NOT FREE ATTRIBUTE -->
  <xsl:template match="o[not(eo:void(.)) and not(@name=$eo:lambda)]" mode="tree">
    <line>
      <xsl:variable name="head">
        <xsl:apply-templates select="." mode="head"/>
      </xsl:variable>
      <xsl:variable name="suffix">
        <xsl:apply-templates select="." mode="tail"/>
      </xsl:variable>
      <xsl:attribute name="base" select="eo:printable(string($head))"/>
      <xsl:attribute name="tail" select="eo:printable(string($suffix))"/>
      <xsl:attribute name="abstract">
        <xsl:value-of select="if (eo:abstract(.) and not(eo:has-data(.))) then 'yes' else 'no'"/>
      </xsl:attribute>
      <xsl:attribute name="test">
        <xsl:value-of select="if (eo:test-attr(.)) then 'yes' else 'no'"/>
      </xsl:attribute>
      <xsl:attribute name="reversed">
        <xsl:value-of select="if (@base and starts-with(@base, '.')) then 'yes' else 'no'"/>
      </xsl:attribute>
      <xsl:apply-templates select="o[not(eo:void(.))]" mode="tree"/>
    </line>
  </xsl:template>
  <!-- BASED -->
  <xsl:template match="o[@base and not(eo:has-data(.))]" mode="head">
    <!-- A base of the form "ξ.ρ.name…" is a single parent hop onto a
         name that lives in the enclosing scope. -->
    <xsl:variable name="rho-prefix" select="concat($eo:xi, '.', $eo:rho, '.')"/>
    <xsl:variable name="rest" select="substring-after(@base, $rho-prefix)"/>
    <xsl:variable name="first" select="if (contains($rest, '.')) then substring-before($rest, '.') else $rest"/>
    <xsl:choose>
      <!-- NOT OPTIMIZED TUPLE -->
      <xsl:when test="@star">
        <xsl:text>*</xsl:text>
      </xsl:when>
      <xsl:when test="@base=$eo:bottom">
        <xsl:text>T</xsl:text>
      </xsl:when>
      <xsl:when test="starts-with(@base, concat($eo:program, '.')) and exists(ancestor::o/o[@name = eo:root-name(current()/@base)])">
        <!-- The plain top-level name would be shadowed by an in-scope
             attribute, so keep the explicit Q. root to disambiguate. -->
        <xsl:value-of select="concat('Q.', eo:translate-path(substring-after(@base, concat($eo:program, '.'))))"/>
      </xsl:when>
      <xsl:when test="starts-with(@base, $rho-prefix) and $first != '' and $first != $eo:rho and $first != $eo:phi and $first != $eo:xi and $first != $eo:program and empty(ancestor::o[not(@base)][1]/o[@name=$first]) and exists(ancestor::o[not(@base)][2]/o[@name=$first])">
        <!-- The single leading "^." parent hop is redundant: the name is
             absent from the immediate scope but present in its parent, so
             plain scope resolution re-derives the very same hop. Drop it. -->
        <xsl:value-of select="eo:translate-path($rest)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="eo:surface(@base)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- ABSTRACT OR ATOM -->
  <xsl:template match="o[eo:abstract(.) and not(eo:has-data(.))]" mode="head">
    <!-- A test attribute with no void params collapses its empty `[]`
         head into the `++&gt;` suffix (see the tail template), so it emits
         no head of its own. -->
    <xsl:if test="not(eo:test-attr(.) and empty(o[eo:void(.)]))">
      <xsl:text>[</xsl:text>
      <xsl:for-each select="o[eo:void(.)]">
        <xsl:if test="position()&gt;1">
          <xsl:text> </xsl:text>
        </xsl:if>
        <xsl:value-of select="@name"/>
      </xsl:for-each>
      <xsl:text>]</xsl:text>
    </xsl:if>
  </xsl:template>
  <!-- TAIL: SUFFIX, NAME, CONST, ATOM -->
  <xsl:template match="o" mode="tail">
    <xsl:if test="@as">
      <xsl:text>:</xsl:text>
      <xsl:choose>
        <xsl:when test="starts-with(@as, $eo:alpha)">
          <xsl:value-of select="substring-after(@as, $eo:alpha)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@as"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:if test="@name">
      <xsl:choose>
        <xsl:when test="eo:test-attr(.)">
          <xsl:choose>
            <!-- No void params: collapse the empty `[]` head into a
                 single `++&gt; name` head (the head template emits
                 nothing in this case). -->
            <xsl:when test="empty(o[eo:void(.)])">
              <xsl:text>++&gt; </xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text> +&gt; </xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:value-of select="substring-after(@name, '+')"/>
        </xsl:when>
        <xsl:when test="starts-with(@name, concat('a', $eo:cactoos))">
          <xsl:text> &gt;&gt;</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text> &gt; </xsl:text>
          <xsl:choose>
            <xsl:when test="@name = $eo:phi">
              <xsl:value-of select="'@'"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="@name"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="@const">
        <xsl:text>!</xsl:text>
      </xsl:if>
      <xsl:if test="eo:atom(.)">
        <xsl:text> /</xsl:text>
        <xsl:variable name="lambda-atom" select="string(./o[@name=$eo:lambda]/@atom)"/>
        <xsl:choose>
          <xsl:when test="starts-with($lambda-atom, 'Φ.')">
            <xsl:variable name="rest" select="substring-after($lambda-atom, 'Φ.')"/>
            <xsl:choose>
              <!--
                A multi-segment signature (e.g. "Φ.malloc.of.allocated") must
                stay rooted: on reparse a dotted @atom is not re-homed, so
                dropping the root would yield an XSD-invalid fqn. A single-name
                signature (e.g. "Φ.bytes") re-resolves to its root on reparse,
                so the implicit root is dropped for idiomatic output.
              -->
              <xsl:when test="contains($rest, '.')">
                <xsl:value-of select="concat('Q.', $rest)"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="$rest"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$lambda-atom"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:if>
    </xsl:if>
  </xsl:template>
  <!-- DATA -->
  <xsl:template match="o[eo:has-data(.)]" mode="head">
    <xsl:value-of select="eo:read-data(.)"/>
  </xsl:template>
</xsl:stylesheet>
