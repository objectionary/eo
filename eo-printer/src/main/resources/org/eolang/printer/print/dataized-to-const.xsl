<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" exclude-result-prefixes="eo" id="dataized-to-const" version="3.0">
  <!--
  Performs the reverse operation of "/org/eolang/parser/const-to-dataized.xsl"
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@base='.as-bytes' and o[position()=1 and @base='Φ.dataized']]">
    <xsl:variable name="argument" select="o[position()=1]/o[1]"/>
    <xsl:choose>
      <xsl:when test="exists($argument)">
        <o>
          <!--
          The readable handle of a const (`foo 42 &gt;&gt;! saved`) stays only
          while the const is still that handle, under its cactus name. Once
          "inline-cactoos" folded it into its only reader, a named binding such
          as `saved &gt; @`, the const takes the reader's name, and keeping the
          handle would print `foo 42 &gt;&gt; saved!` in place of
          `foo 42 &gt; @!`, which loses the reader's name (#9163).
          -->
          <xsl:variable name="named" select="@name and @name != '' and not(starts-with(@name, concat('a', $eo:cactoos)))"/>
          <xsl:apply-templates select="$argument/@*[name()!='as' and not($named and name()='local')]"/>
          <!--
          Named const (a > b!) keeps its name; an anonymous inline const
          argument (42.plus a!) folds in without one and reads as `a!`.
          -->
          <xsl:if test="@name and @name != ''">
            <xsl:attribute name="name" select="@name"/>
          </xsl:if>
          <!--
          Carry the wrapper's positional slot (@as) so a const argument
          beside others (42.plus a! b) keeps its place in the sequence.
          -->
          <xsl:if test="@as">
            <xsl:attribute name="as" select="@as"/>
          </xsl:if>
          <xsl:attribute name="const"/>
          <xsl:for-each select="$argument/o">
            <xsl:apply-templates select="."/>
          </xsl:for-each>
          <xsl:if test="eo:has-data($argument)">
            <xsl:value-of select="eo:read-data($argument)"/>
          </xsl:if>
        </o>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
