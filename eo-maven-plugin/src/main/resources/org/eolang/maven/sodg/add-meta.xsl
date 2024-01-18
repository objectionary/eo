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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="add-meta" version="2.0">
  <!--
  Here we add a vertex with meta information and attach it to v0
  by the attributes that start with "+".
  -->
  <xsl:import href="/org/eolang/maven/sodg/_macros.xsl"/>
  <xsl:param name="name"/>
  <xsl:param name="value"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/sodg">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:variable name="v">
        <xsl:text>meta-</xsl:text>
        <xsl:value-of select="$name"/>
      </xsl:variable>
      <xsl:call-template name="i">
        <xsl:with-param name="name" select="'ADD'"/>
        <xsl:with-param name="args" as="item()*">
          <xsl:sequence>
            <xsl:value-of select="eo:var($v)"/>
          </xsl:sequence>
        </xsl:with-param>
        <xsl:with-param name="comment">
          <xsl:text>This is a meta</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="i">
        <xsl:with-param name="name" select="'BIND'"/>
        <xsl:with-param name="args" as="item()*">
          <xsl:sequence>
            <xsl:text>ν0</xsl:text>
          </xsl:sequence>
          <xsl:sequence>
            <xsl:value-of select="eo:var($v)"/>
          </xsl:sequence>
          <xsl:sequence>
            <xsl:value-of select="concat('+', $name)"/>
          </xsl:sequence>
        </xsl:with-param>
        <xsl:with-param name="comment">
          <xsl:text>This is a meta</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="i">
        <xsl:with-param name="name" select="'PUT'"/>
        <xsl:with-param name="args" as="item()*">
          <xsl:sequence>
            <xsl:value-of select="eo:var($v)"/>
          </xsl:sequence>
          <xsl:sequence>
            <xsl:value-of select="replace($value, ' ', '-')"/>
          </xsl:sequence>
        </xsl:with-param>
        <xsl:with-param name="comment">
          <xsl:text>This is a meta</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:copy>
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
