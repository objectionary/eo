<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="recursion-to-loop" version="2.0">
  <!--
  Here we mark every nested formation that calls itself in a tail position
  with @loop="true", and each of those calls with @again="true", so that
  "to-java" wraps the copies of the formation into "PhLoop" and the calls into
  "PhAgain", and the recursion runs as a loop at run time instead of growing
  the Java stack by one level per iteration (see #5783).

  A formation "F" qualifies when its "φ" is bound, it has no "λ" of its own,
  and its body never refers to its own "φ" (that would open a way to the call
  other than the one through the loop). A call "ξ.ρ.F" is in a tail position
  when every step from the root of the "φ" expression down to it is one of
  two: a branch of an ".if" (the argument α0 or α1, never the receiver), or
  the last element of the tuple that a "seq" is applied to, which after
  "stars-to-tuples" is the α1 of the "Φ.tuple" standing as the α0 of the
  "Φ.seq". Both "bool.if" and "seq" answer exactly that element, so the value
  of the whole body is the value of the call whenever the call is forced,
  and continuing with the next copy in place of the current one is sound.
  The operands of ".or" and ".and" are not tail positions: "bytes.or" is
  strict, and the receiver is not known here to be a "bool".

  Only nested formations are marked, since a top-level object is a class of
  its own that no decorator can wrap, and a call to it is spelled "Φ.name"
  anyway. A self-call outside of a tail position stays what it is: the inner
  copy runs a loop of its own.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  Is the object in a tail position of the expression rooted at $root?
  -->
  <xsl:function name="eo:tail" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:param name="root" as="element()"/>
    <xsl:variable name="parent" as="element()?" select="$o/parent::o"/>
    <xsl:choose>
      <xsl:when test="$o is $root">
        <xsl:sequence select="true()"/>
      </xsl:when>
      <xsl:when test="empty($parent)">
        <xsl:sequence select="false()"/>
      </xsl:when>
      <xsl:when test="($parent/@base = '.if' or ends-with($parent/@base, '.if')) and $o/@as = ('α0', 'α1')">
        <xsl:sequence select="eo:tail($parent, $root)"/>
      </xsl:when>
      <xsl:when test="$parent/@base = 'Φ.tuple' and $parent/@as = 'α0' and $parent/parent::o/@base = 'Φ.seq' and $o/@as = 'α1'">
        <xsl:sequence select="eo:tail($parent/parent::o, $root)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="false()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:template match="abstract[@name and attr[@name='φ']/bound/o and not(attr[@name='λ']) and not(.//o[contains(@base, 'φ')])]">
    <xsl:variable name="root" select="attr[@name='φ']/bound/o"/>
    <xsl:variable name="self" select="concat('ξ.ρ.', @name)"/>
    <xsl:variable name="tails" select="$root//o[@base=$self and eo:tail(., $root)]"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:if test="exists($tails)">
        <xsl:attribute name="loop">true</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="node()">
        <xsl:with-param name="tails" select="$tails" tunnel="yes"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[@base]">
    <xsl:param name="tails" as="element()*" select="()" tunnel="yes"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:if test="exists($tails intersect .)">
        <xsl:attribute name="again">true</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
