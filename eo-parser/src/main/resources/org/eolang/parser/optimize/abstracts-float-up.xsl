<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2024 Objectionary.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="abstracts-float-up" version="2.0">
  <!--
  Here, we extract abstracts that are nested within other abstracts
  and relocate them to the highest level. Example:

  [a] > foo
    [] > test
      3.14 > pi
      "Hello, world"

  Will change to:

  [] > foo$test
    3.14 > pi
    "Hello, world"
  [a] > foo
    foo$test > test
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- The eo:name-of function generates the name of the object based on its
  ancestors. Example:

  [] > another
    eq. > @
      [] > first
        1 > @
      [] > second
        2 > @

   Will generate the following names:
   eo:name-of(first) = "another$t0$first"
   eo:name-of(second) = "another$t0$second"
  -->
  <xsl:function name="eo:name-of" as="xs:string">
    <xsl:param name="object" as="element()"/>
    <xsl:variable name="name">
      <xsl:for-each select="$object/ancestor-or-self::o">
        <xsl:choose>
          <xsl:when test="eo:abstract(.) and not(@name)">
            <xsl:text>a</xsl:text>
            <xsl:value-of select="count(preceding-sibling::o)"/>
          </xsl:when>
          <xsl:when test="eo:abstract(.)">
            <xsl:value-of select="@name"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>t</xsl:text>
            <xsl:value-of select="count(preceding-sibling::o)"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:if test="position() != last()">
          <xsl:text>$</xsl:text>
        </xsl:if>
      </xsl:for-each>
    </xsl:variable>
    <xsl:value-of select="$name"/>
  </xsl:function>
  <!-- Generates pseudo-random integer -->
  <xsl:function name="eo:random-int" as="xs:integer">
    <xsl:variable name="generated">
      <random/>
    </xsl:variable>
    <xsl:value-of select="sum(string-to-codepoints(generate-id($generated//random)))"/>
  </xsl:function>
  <!-- Finds all required attributes -->
  <xsl:function name="eo:vars">
    <xsl:param name="bottom" as="element()"/>
    <xsl:param name="top" as="element()"/>
    <xsl:for-each select="$bottom/ancestor::o[eo:abstract(.)]">
      <xsl:variable name="current-ancestor" select="."/>
      <xsl:if test="$top/descendant-or-self::o[generate-id() = generate-id($current-ancestor)]">
        <xsl:for-each select="$current-ancestor/o[@name and generate-id() != generate-id($bottom)]">
          <xsl:variable name="copied" select="."/>
          <xsl:if test="not($bottom/ancestor-or-self::o[generate-id() = generate-id($copied)]) and $bottom/descendant::o[@base=$copied/@name]">
            <xsl:copy-of select="$copied"/>
          </xsl:if>
        </xsl:for-each>
      </xsl:if>
    </xsl:for-each>
  </xsl:function>
  <xsl:template match="//objects">
    <xsl:copy>
      <xsl:apply-templates select=".//o[eo:abstract(.)]" mode="top"/>
      <xsl:apply-templates select="o[not(eo:abstract(.))]|@*"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[eo:abstract(.)]">
    <xsl:element name="o">
      <xsl:apply-templates select="@* except @base except @abstract"/>
      <xsl:attribute name="base">
        <xsl:value-of select="eo:name-of(.)"/>
      </xsl:attribute>
      <xsl:attribute name="ref">
        <xsl:value-of select="@line"/>
      </xsl:attribute>
      <xsl:attribute name="cut">
        <xsl:value-of select="count(preceding::o)"/>
      </xsl:attribute>
      <xsl:variable name="ancestors" select="ancestor-or-self::o[eo:abstract(.)]"/>
      <xsl:for-each select="1 to count($ancestors) - 1">
        <xsl:variable name="index" select="position()"/>
        <xsl:for-each select="eo:vars($ancestors[count($ancestors) - $index + 1], $ancestors[count($ancestors) - $index])">
          <xsl:element name="o">
            <xsl:attribute name="as">
              <xsl:value-of select="@name"/>
            </xsl:attribute>
            <xsl:attribute name="base">
              <xsl:value-of select="@name"/>
            </xsl:attribute>
            <xsl:attribute name="level">
              <xsl:value-of select="$index"/>
            </xsl:attribute>
          </xsl:element>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>
  <xsl:template match="o[eo:abstract(.)]" mode="top">
    <xsl:variable name="o" select="."/>
    <xsl:copy>
      <xsl:attribute name="name">
        <xsl:value-of select="eo:name-of(.)"/>
      </xsl:attribute>
      <xsl:if test="@name">
        <xsl:attribute name="original-name">
          <xsl:value-of select="@name"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:variable name="ancestors" select="ancestor-or-self::o[eo:abstract(.)]"/>
      <xsl:if test="count($ancestors) &gt; 1">
        <xsl:attribute name="ancestors">
          <xsl:value-of select="count($ancestors) - 1"/>
        </xsl:attribute>
        <xsl:attribute name="parent">
          <xsl:value-of select="eo:name-of($ancestors[last() - 1])"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="node()|@* except @name"/>
      <xsl:for-each select="1 to count($ancestors) - 1">
        <xsl:variable name="index" select="position()"/>
        <xsl:for-each select="eo:vars($ancestors[count($ancestors) - $index + 1], $ancestors[count($ancestors) - $index])">
          <xsl:element name="o">
            <xsl:attribute name="name">
              <xsl:value-of select="@name"/>
            </xsl:attribute>
            <xsl:attribute name="level">
              <xsl:value-of select="$index"/>
            </xsl:attribute>
            <xsl:attribute name="line">
              <xsl:value-of select="$o/@line"/>
              <xsl:text>.</xsl:text>
              <xsl:value-of select="@line"/>
              <xsl:text>.</xsl:text>
              <xsl:value-of select="eo:random-int()"/>
            </xsl:attribute>
          </xsl:element>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
