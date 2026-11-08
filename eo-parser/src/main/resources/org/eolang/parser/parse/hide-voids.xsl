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

  The "φ" void and the "ρ" receiver are never renamed: reads go through "φ"
  on purpose, and the receiver is looked up by its name.

  The locators are set after this step, so they hold the hidden names: a
  locator is the φ path to an object, and in φ the void is called "v🌵x".
  Nothing renames a name after this step. A path that climbs above the top
  of the file stays as written. A member of a package is a file of its own,
  so it reads its object from outside, through getters, and its read of a
  void of the object fails at runtime, like any read from outside.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <!-- Whether the name may take the "v🌵" mark. -->
  <xsl:function name="eo:hideable" as="xs:boolean">
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="$name != $eo:phi and $name != $eo:rho and not(matches($name, concat('^', $eo:alpha, '[0-9]+$')))"/>
  </xsl:function>
  <xsl:template match="o[@base=$eo:empty and parent::o[not(@base)]]/@name[eo:hideable(string(.))]">
    <xsl:attribute name="name" select="concat($eo:veiled, .)"/>
  </xsl:template>
  <xsl:template match="o/@as[eo:hideable(string(.))]">
    <xsl:attribute name="as" select="concat($eo:veiled, .)"/>
  </xsl:template>
  <xsl:template match="o/@base[starts-with(., concat($eo:xi, '.')) or starts-with(., '.')]">
    <xsl:variable name="name" select="eo:own-name(..)"/>
    <xsl:choose>
      <xsl:when test="eo:hideable($name) and exists(eo:own-formation(..)/o[@base=$eo:empty and @name=$name])">
        <xsl:attribute name="base" select="replace(., '^((' || $eo:xi || '(\.' || $eo:rho || ')*)?\.)', '$1' || $eo:veiled)"/>
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
