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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="connect-dots" version="2.0">
  <!--
  Here we find objects with "dot notation" in the @base
  attribute and attach a proper ATOM to their vertices.
  -->
  <xsl:import href="/org/eolang/maven/sodg/_macros.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/sodg">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:apply-templates select="/program/objects//o" mode="sodg"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[@base and not(@level) and starts-with(@base, '.')]" mode="sodg" priority="1">
    <xsl:variable name="v1">
      <xsl:value-of select="@loc"/>
      <xsl:text>.β1</xsl:text>
    </xsl:variable>
    <xsl:variable name="v2">
      <xsl:value-of select="@loc"/>
      <xsl:text>.β2</xsl:text>
    </xsl:variable>
    <xsl:for-each select="$v1, $v2">
      <xsl:call-template name="i">
        <xsl:with-param name="name" select="'ADD'"/>
        <xsl:with-param name="args" as="item()*">
          <xsl:sequence>
            <xsl:value-of select="eo:var(.)"/>
          </xsl:sequence>
        </xsl:with-param>
        <xsl:with-param name="comment">
          <xsl:text>Linking point</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:for-each>
    <xsl:call-template name="i">
      <xsl:with-param name="name" select="'BIND'"/>
      <xsl:with-param name="args" as="item()*">
        <xsl:sequence>
          <xsl:value-of select="eo:var($v2)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:value-of select="eo:var($v1)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:text>β</xsl:text>
        </xsl:sequence>
      </xsl:with-param>
      <xsl:with-param name="comment">
        <xsl:text>This is a dot-notation</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="i">
      <xsl:with-param name="name" select="'BIND'"/>
      <xsl:with-param name="args" as="item()*">
        <xsl:sequence>
          <xsl:value-of select="eo:var($v1)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:value-of select="eo:var(o[1]/@loc)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:variable name="attr">
            <xsl:value-of select="substring(@base, 2)"/>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="$attr='^'">
              <xsl:text>ρ</xsl:text>
            </xsl:when>
            <xsl:when test="$attr='&amp;'">
              <xsl:text>σ</xsl:text>
            </xsl:when>
            <xsl:when test="$attr='&lt;'">
              <xsl:text>ν</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$attr"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:sequence>
      </xsl:with-param>
      <xsl:with-param name="comment">
        <xsl:text>This is a dot-notation</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="i">
      <xsl:with-param name="name" select="'BIND'"/>
      <xsl:with-param name="args" as="item()*">
        <xsl:sequence>
          <xsl:value-of select="eo:var(@loc)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:value-of select="eo:var($v2)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:choose>
            <xsl:when test="count(o) &gt; 1">
              <xsl:text>π</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>ε</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:sequence>
      </xsl:with-param>
      <xsl:with-param name="comment">
        <xsl:text>This is a dot-notation</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="i">
      <xsl:with-param name="name" select="'BIND'"/>
      <xsl:with-param name="args" as="item()*">
        <xsl:sequence>
          <xsl:value-of select="eo:var($v2)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:value-of select="eo:var(o[1]/@loc)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:text>ρ</xsl:text>
        </xsl:sequence>
      </xsl:with-param>
      <xsl:with-param name="comment">
        <xsl:text>This is a dot-notation</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="o" mode="sodg">
    <!-- ignore them -->
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*" mode="#current"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
