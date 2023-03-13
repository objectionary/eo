<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2023 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="bind-rho-and-sigma" version="2.0">
  <!--
  Here we BIND all object formations to their parents using \rho and \sigma.
  We don't bind "data" objects, through.
  -->
  <xsl:import href="/org/eolang/maven/sodg/_macros.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/sodg">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:apply-templates select="/program/objects//o" mode="sodg"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[not(starts-with(@base, '.')) and o or @abstract or (not(@base) and @name)]" mode="sodg" priority="1">
    <xsl:if test="not(@loc)">
      <xsl:message terminate="yes">
        <xsl:text>The object doesn't have @loc, how come?</xsl:text>
      </xsl:message>
    </xsl:if>
    <xsl:call-template name="bind">
      <xsl:with-param name="kid" select="@loc"/>
      <xsl:with-param name="parent" select="ancestor::*[@abstract or name()='objects'][1]/@loc"/>
    </xsl:call-template>
  </xsl:template>
  <xsl:template name="bind">
    <xsl:param name="kid" as="xs:string"/>
    <xsl:param name="parent" as="xs:string"/>
    <xsl:for-each select="('ρ', 'σ')">
      <xsl:call-template name="i">
        <xsl:with-param name="name" select="'BIND'"/>
        <xsl:with-param name="args" as="item()*">
          <xsl:sequence>
            <xsl:value-of select="eo:var($kid)"/>
          </xsl:sequence>
          <xsl:sequence>
            <xsl:value-of select="eo:var($parent)"/>
          </xsl:sequence>
          <xsl:sequence>
            <xsl:value-of select="."/>
          </xsl:sequence>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="o" mode="sodg">
    <!-- ignore it -->
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*" mode="#current"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
