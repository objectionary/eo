<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="_recursion" version="2.0">
  <!--
  Functions shared by "recursion-to-cps.xsl" and "recursion-to-loop.xsl",
  the two stages that turn the self-recursion of a formation into a loop
  (see #5783). Both need the same notion of a tail position, so it lives
  here and is imported by each of them.
  -->
  <!--
  Is the object in a tail position of the expression rooted at $root? It is
  when every step from $root down to it is one of two: a branch of an ".if"
  (the argument α0 or α1, never the receiver), or the last element of the
  tuple that a "seq" is applied to, which after "stars-to-tuples" is the α1
  of the "Φ.tuple" standing as the α0 of the "Φ.seq". Both "bool.if" and
  "seq" answer exactly that element, so the value of the whole expression is
  the value of the object whenever the object is forced. The operands of
  ".or" and ".and" are not tail positions: "bytes.or" is strict, and the
  receiver is not known here to be a "bool".
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
</xsl:stylesheet>
