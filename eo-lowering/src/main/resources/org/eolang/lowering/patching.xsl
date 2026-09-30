<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="patching" version="3.0" exclude-result-prefixes="eo xs">
  <!--
  Here we put an atom in the place of the body of every formation of one
  XMIR file whose entry was rendered into Java. The "φ" of such a formation
  becomes the atom itself, standing where the old "φ" stood, so the
  transpiler names its class after the formation, with "φ" at the end.
  Every other binding of the formation, its voids, its nested formations and
  its tests, stays as it was written, and so does every formation whose entry
  was not rendered. What the old "φ" held goes with it, since nothing but the
  atom computes it now. The "λ" of the atom has no "@atom" type, unlike the
  "λ" of an atom written by hand, and this is how the patching tells the
  atoms it made from all the others.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:param name="rendered" as="xs:string"/>
  <xsl:variable name="eo:locators" as="xs:string*" select="for $row in tokenize(unparsed-text($rendered, 'UTF-8'), '\r?\n')[. != ''] return tokenize($row, '\t')[2]"/>
  <xsl:template match="o[not(@base)][@loc = $eo:locators]/o[@name = 'φ']">
    <o>
      <xsl:copy-of select="@line | @pos"/>
      <xsl:attribute name="loc" select="concat(../@loc, '.φ')"/>
      <xsl:attribute name="name" select="'φ'"/>
      <o>
        <xsl:copy-of select="@line | @pos"/>
        <xsl:attribute name="loc" select="concat(../@loc, '.φ.λ')"/>
        <xsl:attribute name="name" select="'λ'"/>
      </o>
    </o>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
