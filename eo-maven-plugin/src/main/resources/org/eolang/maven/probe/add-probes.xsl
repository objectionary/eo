<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="add-probes" version="2.0" exclude-result-prefixes="eo xs">
  <!--
  For every object FQN add probe meta
  with fully qualified name of the object.
  For instance, for object:
   <o base=".abc" ... >
     <o base=".edf" ... >
       <o base="Q.number"/>
     </o>
   </o>

  The next metas will be added:
   <meta>
     <head>probe</head>
     <tail>Φ.org</tail>
     <part>Φ.org</part>
   </meta>
   <meta>
     <head>probe</head>
     <tail>Φ.org.number</tail>
     <part>Φ.org.number</part>
   </meta>
   <meta>
     <head>probe</head>
     <tail>Φ.org.number.edf</tail>
     <part>Φ.org.number.edf</part>
   </meta>
   <meta>
     <head>probe</head>
     <tail>Φ.org.number.edf.abc</tail>
     <part>Φ.org.number.edf.abc</part>
   </meta>
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:function name="eo:contains-any-of" as="xs:boolean">
    <xsl:param name="original" as="xs:string"/>
    <xsl:param name="chars" as="xs:string*"/>
    <xsl:sequence select="some $char in $chars satisfies contains($original, $char)"/>
  </xsl:function>
  <xsl:function name="eo:meta">
    <xsl:param name="arg"/>
    <xsl:element name="meta">
      <xsl:element name="head">
        <xsl:text>probe</xsl:text>
      </xsl:element>
      <xsl:element name="tail">
        <xsl:value-of select="$arg"/>
      </xsl:element>
      <xsl:element name="part">
        <xsl:value-of select="$arg"/>
      </xsl:element>
    </xsl:element>
  </xsl:function>
  <!-- ENTRY POINT 1 - no metas -->
  <xsl:template match="/object[not(metas)]">
    <xsl:variable name="candidates" as="element()*">
      <xsl:apply-templates select="//o[not(eo:abstract(.)) and not(eo:void(.))]" mode="create"/>
    </xsl:variable>
    <xsl:variable name="probes" select="distinct-values($candidates/text())[not(eo:contains-any-of(., ('ξ', 'ρ', 'φ'))) and not(.='Φ') and not(.='Φ̇')]"/>
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:if test="not(empty($probes))">
        <metas>
          <xsl:for-each select="$probes">
            <xsl:copy-of select="eo:meta(.)"/>
          </xsl:for-each>
        </metas>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
  <!-- ENTRY POINT 2 - metas exists -->
  <xsl:template match="/object/metas">
    <xsl:variable name="candidates" as="element()*">
      <xsl:apply-templates select="//o[not(eo:abstract(.)) and not(eo:void(.))]" mode="create"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:for-each select="distinct-values($candidates/text())[not(eo:contains-any-of(., ('ξ', 'ρ', 'φ'))) and not(.='Φ') and not(.='Φ̇')]">
        <xsl:copy-of select="eo:meta(.)"/>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <!-- Composite base -->
  <xsl:template match="o[not(starts-with(@base, '.'))]" mode="create" as="element()*">
    <xsl:variable name="parts" select="tokenize(@base, '\.')"/>
    <xsl:for-each select="$parts">
      <xsl:variable name="pos" select="position()"/>
      <a>
        <xsl:value-of select="string-join($parts[position()&lt;=$pos], '.')"/>
      </a>
    </xsl:for-each>
    <xsl:apply-templates select="o" mode="create"/>
  </xsl:template>
  <!-- Method base -->
  <xsl:template match="o[starts-with(@base, '.')]" mode="create" as="element()*">
    <xsl:variable name="first" select="o[1]"/>
    <xsl:choose>
      <xsl:when test="$first[eo:abstract(.)]">
        <xsl:apply-templates select="o[position()&gt;1]" mode="create"/>
      </xsl:when>
      <xsl:when test="starts-with($first/@base, '.')">
        <xsl:variable name="nested" as="element()*">
          <xsl:apply-templates select="$first" mode="create"/>
        </xsl:variable>
        <xsl:copy-of select="$nested"/>
        <xsl:apply-templates select="o[position()&gt;1]" mode="create"/>
        <xsl:if test="ends-with($nested[last()]/text(), $first/@base)">
          <a>
            <xsl:value-of select="concat($nested[last()]/text(), @base)"/>
          </a>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="$first" mode="create"/>
        <xsl:apply-templates select="o[position()&gt;1]" mode="create"/>
        <a>
          <xsl:value-of select="concat($first/@base, @base)"/>
        </a>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
