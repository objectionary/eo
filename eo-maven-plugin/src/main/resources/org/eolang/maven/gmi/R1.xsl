<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2022 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="R1" version="2.0">
  <!--
  Here we find ADD all objects to the graph and BIND them to
  their parents.
  -->
  <xsl:import href="/org/eolang/maven/gmi/_macros.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/gmi">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:apply-templates select="//o" mode="gmi"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o" mode="gmi" priority="1">
    <xsl:call-template name="i">
      <xsl:with-param name="name" select="'ADD'"/>
      <xsl:with-param name="args" as="item()*">
        <xsl:sequence>
          <xsl:value-of select="eo:vertex(.)"/>
        </xsl:sequence>
      </xsl:with-param>
      <xsl:with-param name="comment">
        <xsl:text>[R1]</xsl:text>
        <xsl:if test="@name">
          <xsl:text> name=</xsl:text>
          <xsl:value-of select="@name"/>
        </xsl:if>
        <xsl:if test="@abstract">
          <xsl:text> abstract</xsl:text>
        </xsl:if>
        <xsl:if test="@base">
          <xsl:text> base=</xsl:text>
          <xsl:value-of select="@base"/>
        </xsl:if>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="i">
      <xsl:with-param name="name" select="'BIND'"/>
      <xsl:with-param name="args" as="item()*">
        <xsl:sequence>
          <xsl:value-of select="eo:edge(ancestor::*[1], .)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:value-of select="eo:vertex(ancestor::*[1])"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:value-of select="eo:vertex(.)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:variable name="r">
            <xsl:choose>
              <xsl:when test="@name">
                <xsl:value-of select="@name"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:text>α</xsl:text>
                <xsl:value-of select="count(preceding-sibling::o)"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          <xsl:value-of select="concat('text:', eo:attr($r))"/>
        </xsl:sequence>
      </xsl:with-param>
      <xsl:with-param name="comment">
        <xsl:text>[R1] The object</xsl:text>
        <xsl:if test="@name">
          <xsl:text> '</xsl:text>
          <xsl:value-of select="@name"/>
          <xsl:text>'</xsl:text>
        </xsl:if>
        <xsl:text> belongs to its owner</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="i">
      <xsl:with-param name="name" select="'BIND'"/>
      <xsl:with-param name="args" as="item()*">
        <xsl:sequence>
          <xsl:variable name="e">
            <xsl:value-of select="eo:edge(ancestor::*[1], .)"/>
            <xsl:text>.rho</xsl:text>
          </xsl:variable>
          <xsl:value-of select="$e"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:value-of select="eo:vertex(.)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:value-of select="eo:vertex(ancestor::*[1])"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:text>text:ρ</xsl:text>
        </xsl:sequence>
      </xsl:with-param>
      <xsl:with-param name="comment">
        <xsl:text>[R1] Reverse link to the owner</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="i">
      <xsl:with-param name="name" select="'BIND'"/>
      <xsl:with-param name="args" as="item()*">
        <xsl:sequence>
          <xsl:variable name="e">
            <xsl:value-of select="eo:edge(ancestor::*[1], .)"/>
            <xsl:text>.sigma</xsl:text>
          </xsl:variable>
          <xsl:value-of select="$e"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:value-of select="eo:vertex(.)"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:value-of select="eo:vertex(ancestor::*[1])"/>
        </xsl:sequence>
        <xsl:sequence>
          <xsl:text>text:𝜎</xsl:text>
        </xsl:sequence>
      </xsl:with-param>
      <xsl:with-param name="comment">
        <xsl:text>[R1] Reverse link to the owner</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="o" mode="gmi">
    <!-- ignore it -->
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*" mode="#current"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
