<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="set-locators" version="2.0">
  <!--
  Here we go through all objects and add @loc attributes
  to all of them. The value of the attribute is a unique locator
  of the object.
  The locator of an object is the locator of its parent plus one more
  segment, so we build them top-down: every "o" receives the locator
  already built for its parent through the "eo:parent" tunnel parameter
  and only appends its own segment. Reaching the program from every
  object instead re-derives all of its ancestors, which costs O(depth)
  per object rather than O(1), and hits the limit on nested calls that
  Saxon imposes once objects nest a few hundred levels deep (#6509).
  The "Φ.package" prefix is likewise computed once, on the "object"
  element, and tunnelled down, instead of being looked up again for
  every object in the document.
  Both values travel as template parameters, not as extra arguments of
  a locator function. Saxon 13.0 binds the arguments of a
  multi-argument function to the wrong values, once in a while, when
  one compiled stylesheet transforms many documents in parallel threads
  (which is what the "parse" goal does), so eo:segment below stays
  single-argument. Nothing here reads the global context item either,
  since that item is absent when the stylesheet is driven through
  xsl:apply-templates rather than through a whole-document transform.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_specials.xsl"/>
  <!--
  The trailing segment of the locator of the given object, without the
  dot that joins it to the locator of its parent.
  -->
  <xsl:function name="eo:segment" as="xs:string">
    <xsl:param name="o" as="element(o)"/>
    <xsl:variable name="dotted" as="xs:boolean" select="starts-with($o/parent::o/@base, '.')"/>
    <xsl:choose>
      <xsl:when test="$o/@name = '@'">
        <xsl:sequence select="$eo:phi"/>
      </xsl:when>
      <xsl:when test="$o/@name">
        <xsl:sequence select="string($o/@name)"/>
      </xsl:when>
      <xsl:when test="$o/@as">
        <xsl:sequence select="string($o/@as)"/>
      </xsl:when>
      <xsl:when test="$dotted and not($o/preceding-sibling::o)">
        <xsl:sequence select="$eo:rho"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="concat($eo:alpha, count($o/preceding-sibling::o) - (if ($dotted) then 1 else 0))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:template match="object">
    <xsl:variable name="pkg" select="metas/meta[head='package'][1]/part[1]"/>
    <xsl:copy>
      <xsl:apply-templates select="node()|@*">
        <xsl:with-param name="eo:parent" as="xs:string" tunnel="yes" select="if ($pkg) then concat($eo:program, '.', $pkg) else $eo:program"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o">
    <xsl:param name="eo:parent" as="xs:string" select="$eo:program" tunnel="yes"/>
    <xsl:variable name="loc" as="xs:string" select="concat($eo:parent, '.', eo:segment(.))"/>
    <xsl:copy>
      <xsl:attribute name="loc" select="$loc"/>
      <xsl:apply-templates select="node()|@* except @loc">
        <xsl:with-param name="eo:parent" as="xs:string" tunnel="yes" select="$loc"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
