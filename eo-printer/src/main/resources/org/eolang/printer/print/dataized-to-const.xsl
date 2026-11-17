<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="dataized-to-const" version="3.0">
  <!--
  Performs the reverse operation of "/org/eolang/parser/const-to-dataized.xsl"
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- Every reference to a name, by the last segment of the base it reads. -->
  <xsl:key name="reader" match="o[@base]" use="tokenize(@base, '\.')[last()]"/>
  <!--
  The formation that "const-to-dataized" moved out of a const and
  "vars-float-up" left beside it, or nothing when the const holds its
  argument itself. A const on a formation (`[] &gt; x!`) cannot keep the
  formation under the wrapper, because the wrapper takes the name `x` and
  two attributes of one name are not allowed, so the formation goes next to
  it as `x-1` and the wrapper only reads it. Printed back as it stands, that
  pair is neither the program the author wrote nor one that parses again:
  the sibling is a public attribute nobody declared, and a pipe below the
  const finds a reference above it rather than the formation it follows
  (#9382). The sibling is recognised by the coordinates it kept from the
  const, by the name the const gave it, and by being read exactly once,
  from inside this very wrapper.
  -->
  <xsl:function name="eo:hidden" as="element(o)?">
    <xsl:param name="wrapper" as="element(o)"/>
    <xsl:variable name="read" select="$wrapper/o[1]/o[1]"/>
    <xsl:variable name="prefix" select="concat($wrapper/@name, '-')"/>
    <xsl:sequence select="$wrapper/../o[$wrapper/@name and @name and not(@base) and @line = $wrapper/@line and @pos = $wrapper/@pos and starts-with(@name, $prefix) and substring-after(@name, $prefix) castable as xs:integer and exists($read) and empty($read/node()) and tokenize($read/@base, '\.')[last()] = @name and count(key('reader', @name)) = 1][1]"/>
  </xsl:function>
  <xsl:template match="o[@base='.as-bytes' and o[position()=1 and @base='Φ.dataized']]">
    <xsl:variable name="hidden" select="eo:hidden(.)"/>
    <xsl:variable name="argument" select="if (exists($hidden)) then $hidden else o[position()=1]/o[1]"/>
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
  <!-- The floated formation itself, dropped where the const took it back. -->
  <xsl:template match="o[@name and not(@base) and (some $wrapper in ../o[@base='.as-bytes' and o[position()=1 and @base='Φ.dataized']] satisfies (eo:hidden($wrapper) is .))]"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
