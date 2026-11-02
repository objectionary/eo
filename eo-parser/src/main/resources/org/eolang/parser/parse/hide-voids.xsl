<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="hide-voids" version="3.0">
  <!--
  Here we hide every void from the readers outside of its formation, while
  leaving it open to be filled by name (#9788). A void "x" is renamed to
  "v🌵x", and so is every own read of it: a path that starts at "ξ" and goes
  only through "ρ" hops, like "ξ.x", "ξ.ρ.x" or "ξ.ρ.ρ.x", or a dispatch of
  "x" made on such a path. A declared receiver "^me" is already a "ρ" hop
  here, since "resolve-local-names" turned "me" into one. Each path is
  followed to the formation it lands in, and its name is renamed only when
  it is a void of that formation. A named binding "5:x" only ever
  fills a void, so its "@as" is renamed without asking what it fills, while
  a positional one keeps its "αN".

  An outside read, like "Φ.bar.x" or a dispatch of "x" on an application,
  stays as written, finds no attribute "x" and fails with the error a typo
  gets. Nobody can write "v🌵x" by hand, since the parser refuses the cactus
  in a name.

  The "φ" void, the "ρ" receiver and any name the compiler made itself
  (one with a cactus inside, such as the "a🌵" name of a "? >> h" void) are
  never renamed: reads go through "φ" on purpose, the receiver is looked up
  by its name, and a cactus name is out of reach already.

  The locators are set after this step: a locator is the φ path to an
  object, and in φ the void is called "v🌵x". A name that already has a
  locator is therefore left as it is, and so is every read of a void with
  such a name, because the read would miss the void otherwise. A name that
  already carries "v🌵" is left as it is too, so a second run changes
  nothing. That lets the merge run the step again over an object that has
  just taken its members in: a member reads a void of the object through
  its receiver, a "ρ" hop that left the member's file before the merge, so
  only now does the read land in the object.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <!--
  Every void, by the name it has once this step is over, together with the
  formation it belongs to: "v🌵x" for a void hidden already or about to be,
  and the name as it stands for one this step leaves alone.
  -->
  <xsl:key name="voids" match="o[@base=$eo:empty and @name and parent::o[not(@base)]]" use="concat(if (eo:hideable(string(@name)) and empty(@loc)) then $eo:veiled else '', @name, '#', generate-id(..))"/>
  <!-- Whether the name may take the "v🌵" mark. -->
  <xsl:function name="eo:hideable" as="xs:boolean">
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="$name != $eo:phi and $name != $eo:rho and not(contains($name, $eo:cactoos)) and not(matches($name, concat('^', $eo:alpha, '[0-9]+$')))"/>
  </xsl:function>
  <!--
  The formation that an object made of nothing but "ξ" and "ρ" hops lands
  in, rolled as "ξ.ρ.ρ" or not as ".ρ" on top of "ξ", or nothing when the
  object is anything else or climbs above the top of the file.
  -->
  <xsl:function name="eo:landing" as="element(o)?">
    <xsl:param name="o" as="element(o)?"/>
    <xsl:variable name="base" select="string($o/@base)"/>
    <xsl:choose>
      <xsl:when test="matches($base, concat('^', $eo:xi, '(\.', $eo:rho, ')*$'))">
        <xsl:sequence select="$o/ancestor::o[eo:abstract(.)][count(tokenize($base, '\.'))]"/>
      </xsl:when>
      <xsl:when test="$base = concat('.', $eo:rho)">
        <xsl:sequence select="eo:landing($o/o[1])/ancestor::o[eo:abstract(.)][1]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:template match="o[@base=$eo:empty and parent::o[not(@base)] and empty(@loc)]/@name[eo:hideable(string(.))]">
    <xsl:attribute name="name" select="concat($eo:veiled, .)"/>
  </xsl:template>
  <xsl:template match="o[empty(@loc)]/@as[eo:hideable(string(.))]">
    <xsl:attribute name="as" select="concat($eo:veiled, .)"/>
  </xsl:template>
  <xsl:template match="o/@base[starts-with(., concat($eo:xi, '.'))]">
    <xsl:variable name="segments" select="tokenize(., '\.')"/>
    <xsl:variable name="place" select="(index-of(for $segment in $segments return $segment = $eo:rho, false())[. &gt; 1])[1]"/>
    <xsl:variable name="name" select="if (exists($place)) then $segments[$place] else ''"/>
    <xsl:variable name="formation" select="if (exists($place)) then ../ancestor::o[eo:abstract(.)][$place - 1] else ()"/>
    <xsl:choose>
      <xsl:when test="eo:hideable($name) and exists($formation) and exists(key('voids', concat($eo:veiled, $name, '#', generate-id($formation))))">
        <xsl:attribute name="base" select="string-join(for $at in 1 to count($segments) return if ($at = $place) then concat($eo:veiled, $segments[$at]) else $segments[$at], '.')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="o/@base[starts-with(., '.')]">
    <xsl:variable name="name" select="substring(., 2)"/>
    <xsl:variable name="formation" select="eo:landing(../o[1])"/>
    <xsl:choose>
      <xsl:when test="eo:hideable($name) and exists($formation) and exists(key('voids', concat($eo:veiled, $name, '#', generate-id($formation))))">
        <xsl:attribute name="base" select="concat('.', $eo:veiled, $name)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
