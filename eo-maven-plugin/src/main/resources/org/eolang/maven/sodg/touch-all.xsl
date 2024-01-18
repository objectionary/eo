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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="touch-all" version="2.0">
  <!--
  Here we find all objects that have @loc attributes (basically, all objects).
  Then we make sure their vertices exist in the graph
  and are bound to their parents.
  -->
  <xsl:import href="/org/eolang/maven/sodg/_macros.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/sodg">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:apply-templates select="/program/objects//o" mode="sodg"/>
    </xsl:copy>
  </xsl:template>
  <xsl:function name="eo:th">
    <xsl:param name="i" as="xs:integer"/>
    <xsl:choose>
      <xsl:when test="$i = 1">
        <xsl:text>1st</xsl:text>
      </xsl:when>
      <xsl:when test="$i = 2">
        <xsl:text>2nd</xsl:text>
      </xsl:when>
      <xsl:when test="$i = 3">
        <xsl:text>3rd</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$i"/>
        <xsl:text>th</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:template match="o[not(@level)]" mode="sodg" priority="1">
    <xsl:call-template name="touch">
      <xsl:with-param name="o" select="."/>
      <xsl:with-param name="loc" select="@loc"/>
    </xsl:call-template>
    <xsl:if test="@base and not(starts-with(@base, '.'))">
      <xsl:variable name="b-loc" select="eo:base-to-loc(.)"/>
      <xsl:if test="$b-loc != @loc">
        <xsl:call-template name="touch">
          <xsl:with-param name="o" select="."/>
          <xsl:with-param name="loc" select="$b-loc"/>
        </xsl:call-template>
      </xsl:if>
    </xsl:if>
  </xsl:template>
  <xsl:template name="touch">
    <xsl:param name="o"/>
    <xsl:param name="loc"/>
    <xsl:variable name="identifiers" select="tokenize($loc, '\.')"/>
    <xsl:for-each select="$identifiers">
      <xsl:variable name="p" select="position()"/>
      <xsl:if test="$p &gt; 1">
        <xsl:variable name="kid">
          <xsl:for-each select="$identifiers">
            <xsl:if test="position() &lt;= $p">
              <xsl:if test="position() &gt; 1">
                <xsl:text>.</xsl:text>
              </xsl:if>
              <xsl:value-of select="."/>
            </xsl:if>
          </xsl:for-each>
        </xsl:variable>
        <xsl:if test="not($o/(preceding::o | ancestor::o)[starts-with(concat(@loc, '.'), concat($kid, '.')) or (@base and not(starts-with(@base, '.')) and starts-with(concat(eo:base-to-loc(.), '.'), concat($kid, '.')))])">
          <xsl:call-template name="add">
            <xsl:with-param name="loc" select="$kid"/>
            <xsl:with-param name="full" select="$o/@loc"/>
          </xsl:call-template>
        </xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="add">
    <xsl:param name="loc"/>
    <xsl:param name="full"/>
    <xsl:variable name="parent" select="eo:parent-of-loc($loc)"/>
    <xsl:call-template name="i">
      <xsl:with-param name="name" select="'ADD'"/>
      <xsl:with-param name="args" as="item()*">
        <xsl:sequence>
          <xsl:value-of select="eo:var($loc)"/>
        </xsl:sequence>
      </xsl:with-param>
      <xsl:with-param name="comment">
        <xsl:text>The </xsl:text>
        <xsl:value-of select="eo:th(position())"/>
        <xsl:text> part of the '</xsl:text>
        <xsl:value-of select="$full"/>
        <xsl:text>' locator, because '</xsl:text>
        <xsl:value-of select="$loc"/>
        <xsl:text>' has not been seen</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:variable name="parts" select="tokenize($loc, '\.')"/>
    <xsl:variable name="k" select="$parts[count($parts)]"/>
    <xsl:if test="$k != 'ρ'">
      <xsl:call-template name="i">
        <xsl:with-param name="name" select="'BIND'"/>
        <xsl:with-param name="args" as="item()*">
          <xsl:sequence>
            <xsl:value-of select="eo:var($parent)"/>
          </xsl:sequence>
          <xsl:sequence>
            <xsl:value-of select="eo:var($loc)"/>
          </xsl:sequence>
          <xsl:sequence>
            <xsl:value-of select="$k"/>
          </xsl:sequence>
        </xsl:with-param>
        <xsl:with-param name="comment">
          <xsl:text>Link to the </xsl:text>
          <xsl:value-of select="eo:th(position())"/>
          <xsl:text> part of the '</xsl:text>
          <xsl:value-of select="$k"/>
          <xsl:text>' locator</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>
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
