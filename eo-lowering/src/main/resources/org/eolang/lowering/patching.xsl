<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:map="http://www.w3.org/2005/xpath-functions/map" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="patching" version="3.0" exclude-result-prefixes="eo map xs">
  <!--
  Here we put an atom in the place of the body of every formation of one
  XMIR file whose entry was rendered into Java. Such a formation gets one
  attribute more, the atom "l🌵N" named after the number N of its entry,
  and its "φ" becomes a dispatch to that atom, standing where the old "φ"
  stood. Every other binding of the formation, its voids, its nested
  formations and its tests, stays as it was written, and so does every
  formation whose entry was not rendered. What the old "φ" held goes with
  it, since nothing but the atom computes it now.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:param name="rendered" as="xs:string"/>
  <xsl:variable name="eo:numbers" as="map(xs:string, xs:string)" select="map:merge(for $row in tokenize(unparsed-text($rendered, 'UTF-8'), '\r?\n')[. != ''] return map {tokenize($row, '\t')[2]: tokenize($row, '\t')[1]})"/>
  <xsl:template match="o[not(@base)][@loc][map:contains($eo:numbers, @loc)]">
    <xsl:variable name="atom" select="concat('l🌵', $eo:numbers(@loc))"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:for-each select="node()">
        <xsl:choose>
          <xsl:when test="self::o[@name = 'φ']">
            <o>
              <xsl:copy-of select="@* except @base"/>
              <xsl:attribute name="base" select="concat('ξ.', $atom)"/>
            </o>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="."/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
      <o>
        <xsl:copy-of select="@line | @pos"/>
        <xsl:attribute name="loc" select="concat(@loc, '.', $atom)"/>
        <xsl:attribute name="name" select="$atom"/>
        <o>
          <xsl:copy-of select="@line | @pos"/>
          <xsl:attribute name="loc" select="concat(@loc, '.', $atom, '.λ')"/>
          <xsl:attribute name="name" select="'λ'"/>
        </o>
      </o>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
