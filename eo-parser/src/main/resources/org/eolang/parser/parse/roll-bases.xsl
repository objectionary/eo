<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="https://www.eolang.org" id="roll-bases" version="2.0">
  <!--

  - <o base=".org">
      <o base="Φ"/>  => <o base="org"/>
    </o>
  - <o base=".instructions">
      <o base="$"/>  => <o base="instructions"/>
    </o>
  - <o base=".eolang">
      <o base="org"/>  => <o base="org.eolang"/>
    </o>
  - <o base=".seq">
      <o base="org.eolang"/>  => <o base="org.eolang.seq"/>
    </o>
  - <o base=".seq">
      <o base=".eolang">
        <o base=".org">
          <o base="Φ"/>   => <o base="Φ.org.eolang.seq"/>
        </o>
      </o>
    </o>
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:template match="o[starts-with(@base, '.')]">
    <xsl:variable name="first" select="o[1]"/>
    <xsl:choose>
      <!--
        x.
          y.
        Process y.
      -->
      <xsl:when test="starts-with($first/@base, '.')">
        <xsl:variable name="argument" as="element()">
          <xsl:apply-templates select="$first"/>
        </xsl:variable>
        <xsl:choose>
          <!--
            Left as
            x.
              y.
            Copy as is
          -->
          <xsl:when test="starts-with($argument/@base, '.')">
            <xsl:element name="o">
              <xsl:apply-templates select="@*"/>
              <xsl:copy-of select="$argument"/>
              <xsl:apply-templates select="node()[position()&gt;1]"/>
            </xsl:element>
          </xsl:when>
          <!--
            Either rolled, but with data:
              x.
                z.y
                  2A-
            Or not rolled because anonymous:
              x.
                []
          -->
          <xsl:when test="not(exists($argument/@base)) or eo:has-data($argument)">
            <xsl:element name="o">
              <xsl:apply-templates select="@*"/>
              <xsl:element name="o">
                <xsl:apply-templates select="$argument/@*"/>
                <xsl:apply-templates select="$argument/node()"/>
              </xsl:element>
              <xsl:apply-templates select="node()[position()&gt;1]"/>
            </xsl:element>
          </xsl:when>
          <!--
            Changed to
            x.
              z.y
             Try to roll again
          -->
          <xsl:otherwise>
            <xsl:apply-templates select="." mode="roll">
              <xsl:with-param name="arg" select="$argument"/>
            </xsl:apply-templates>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <!--
        - With anonymous
        x.
          []

        - Or with data
        x.
          2A-

        - Or ∅ (in case of parsing errors)
        x.
          ∅

        Leave as is
      -->
      <xsl:when test="not(exists($first/@base)) or eo:has-data($first) or eo:void($first)">
        <xsl:element name="o">
          <xsl:apply-templates select="@*"/>
          <xsl:element name="o">
            <xsl:apply-templates select="$first/@*"/>
            <xsl:apply-templates select="$first/node()"/>
          </xsl:element>
          <xsl:apply-templates select="node()[position()&gt;1]"/>
        </xsl:element>
      </xsl:when>
      <!--
        x.
          y
      -->
      <xsl:otherwise>
        <xsl:apply-templates select="." mode="roll">
          <xsl:with-param name="arg" select="$first"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- Try to roll @base with first child -->
  <xsl:template match="o" mode="roll">
    <xsl:param name="arg"/>
    <xsl:choose>
      <!--
        x.
          y
            z
        No rolling because of argument
      -->
      <xsl:when test="$arg/o">
        <xsl:element name="o">
          <xsl:apply-templates select="@*"/>
          <xsl:element name="o">
            <xsl:apply-templates select="$arg/@*"/>
            <xsl:apply-templates select="$arg/node()"/>
          </xsl:element>
          <xsl:apply-templates select="node()[position()&gt;1]"/>
        </xsl:element>
      </xsl:when>
      <!--
        x.
          y
        Roll into y.x
      -->
      <xsl:otherwise>
        <xsl:element name="o">
          <xsl:apply-templates select="@* except @base"/>
          <xsl:attribute name="base">
            <xsl:value-of select="concat($arg/@base, @base)"/>
          </xsl:attribute>
          <xsl:apply-templates select="node()[position()&gt;1]"/>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- Default copying -->
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
