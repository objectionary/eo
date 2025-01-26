<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2025 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="https://www.eolang.org" id="roll-bases" version="2.0">
  <!--
  This XSL rolls ONE reversed object dispatch into single base.
  To get all the dispatches rolled up you need to apply the transformation
  multiple times until it makes no effect.

  - <o base=".org">
      <o base="Q"/>  => <o base="org"/>
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
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:template match="o[starts-with(@base, '.')]">
    <xsl:variable name="first" select="o[1]"/>
    <xsl:choose>
      <!--
        .x
          .y
        Process .y
      -->
      <xsl:when test="starts-with($first/@base, '.')">
        <xsl:variable name="argument" as="element()">
          <xsl:apply-templates select="$first"/>
        </xsl:variable>
        <xsl:choose>
          <!--
            Left as
            .x
              .y
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
            Changed to
            .x
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
        .x
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
        .x
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
          <xsl:if test="eo:has-data(.)">
            <xsl:value-of select="."/>
          </xsl:if>
        </xsl:element>
      </xsl:when>
      <!--
        .x
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
          <xsl:if test="eo:has-data(.)">
            <xsl:value-of select="."/>
          </xsl:if>
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