<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="applying" version="2.0">
  <!--
  Here we spell the way from the root of the universe down to one
  fragment. Every formation on the way is a step, and phino enters the
  fragment by walking them one after another, so the way has to be told
  in the order it is walked, outermost first.
  One line comes out per step: the locator, the name the parent holds it
  by, whether it stands at the top of the document, and the name of every
  argument it declares. What each argument carries is not here, because
  only the inference knows it, and the symbols behind the arguments are
  minted by the build, which owns the table.
  -->
  <xsl:output encoding="UTF-8" method="text"/>
  <!-- The locator of the fragment to walk down to -->
  <xsl:param name="locator" as="xs:string" required="yes"/>
  <!-- One step of the way, as a line of its own -->
  <xsl:function name="eo:step" as="xs:string">
    <xsl:param name="o" as="element(o)"/>
    <xsl:sequence select="concat($o/@loc, '&#9;', $o/@name, '&#9;', if (name($o/..) = 'o') then '0' else '1', '&#9;', string-join($o/o[@base = '∅'][@name != 'ρ']/@name, ' '), '&#10;')"/>
  </xsl:function>
  <xsl:template match="/">
    <xsl:value-of select="for $o in (//o[@loc = $locator])[1]/ancestor-or-self::o[empty(@base)] return eo:step($o)" separator=""/>
  </xsl:template>
</xsl:stylesheet>
