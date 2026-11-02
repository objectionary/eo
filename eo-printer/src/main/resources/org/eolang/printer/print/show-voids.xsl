<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="show-voids" version="3.0">
  <!--
  Inverse of the parser's "hide-voids" pass, applied before every other
  printing step (#9788). The parser renames a void "x" to "v🌵x", together
  with every own read of it and every named binding "5:x", and here the
  "v🌵" mark comes off all of them again, so the steps that follow see the
  names the source wrote.

  An own read is a path that starts at "ξ" and goes only through "ρ" hops,
  like "ξ.v🌵x" or "ξ.ρ.v🌵x", or a dispatch of "v🌵x" made on such a path.
  Any other read of a "v🌵" name reads a void from outside of its
  formation, which EO has no way to write, so printing stops there.

  A formation that holds a filled hidden void, the way a simplified
  "⟦ v🌵x ↦ 5, value ↦ ξ.v🌵x ⟧" does, gets its void "x" back, while the
  object that filled it moves to the end of the formation as the named
  binding "5:x".
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  Whether the object is made of nothing but "ξ" and "ρ" hops, rolled as
  "ξ.ρ.ρ" or not as ".ρ" on top of "ξ".
  -->
  <xsl:function name="eo:own" as="xs:boolean">
    <xsl:param name="o" as="element(o)?"/>
    <xsl:variable name="base" select="string($o/@base)"/>
    <xsl:sequence select="matches($base, concat('^', $eo:xi, '(\.', $eo:rho, ')*$')) or ($base = concat('.', $eo:rho) and eo:own($o/o[1]))"/>
  </xsl:function>
  <!-- The name without its "v🌵" mark. -->
  <xsl:function name="eo:shown" as="xs:string">
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="if (starts-with($name, $eo:veiled)) then substring-after($name, $eo:veiled) else $name"/>
  </xsl:function>
  <!-- Whether the object is a filled hidden void of a formation. -->
  <xsl:function name="eo:filled" as="xs:boolean">
    <xsl:param name="o" as="element(o)"/>
    <xsl:sequence select="starts-with(string($o/@name), $eo:veiled) and not($o/@base = $eo:empty) and exists($o/parent::o[eo:abstract(.)])"/>
  </xsl:function>
  <xsl:template match="o[eo:abstract(.) and o[eo:filled(.)]]">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:apply-templates select="o[eo:filled(.)]" mode="fill"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[eo:filled(.)]" priority="2">
    <xsl:copy>
      <xsl:apply-templates select="@* except (@base, @name, @const)"/>
      <xsl:attribute name="base" select="$eo:empty"/>
      <xsl:attribute name="name" select="eo:shown(string(@name))"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o" mode="fill">
    <xsl:copy>
      <xsl:apply-templates select="@* except (@name, @loc)"/>
      <xsl:attribute name="as" select="eo:shown(string(@name))"/>
      <xsl:apply-templates select="node()"/>
      <xsl:apply-templates select="o[eo:filled(.)]" mode="fill"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o/@name[starts-with(., $eo:veiled)]">
    <xsl:attribute name="name" select="eo:shown(string(.))"/>
  </xsl:template>
  <xsl:template match="o/@as[starts-with(., $eo:veiled)]">
    <xsl:attribute name="as" select="eo:shown(string(.))"/>
  </xsl:template>
  <xsl:template match="o/@base[contains(., $eo:veiled)]">
    <xsl:variable name="segments" select="tokenize(., '\.')"/>
    <xsl:variable name="place" select="(index-of(for $segment in $segments return $segment = $eo:rho, false())[. &gt; 1])[1]"/>
    <xsl:variable name="hidden" select="index-of(for $segment in $segments return contains($segment, $eo:veiled), true())"/>
    <xsl:choose>
      <xsl:when test="$segments[1] = $eo:xi and count($hidden) = 1 and $hidden = $place">
        <xsl:attribute name="base" select="string-join(for $segment in $segments return eo:shown($segment), '.')"/>
      </xsl:when>
      <xsl:when test="starts-with(., concat('.', $eo:veiled)) and count($hidden) = 1 and eo:own(../o[1])">
        <xsl:attribute name="base" select="concat('.', eo:shown(substring(., 2)))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">
          <xsl:text>Can't print "</xsl:text>
          <xsl:value-of select="."/>
          <xsl:text>" at </xsl:text>
          <xsl:value-of select="if (../@loc) then ../@loc else concat('line ', ../@line)"/>
          <xsl:text>, because it reads a void from outside of its formation, which EO has no way to write</xsl:text>
        </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
