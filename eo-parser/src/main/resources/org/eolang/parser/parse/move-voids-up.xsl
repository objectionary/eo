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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="move-voids-up" version="2.0">
  <!--
  Move void attributes on top
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[count(o)&gt;0]">
    <xsl:apply-templates select="." mode="with-attrs"/>
  </xsl:template>
  <xsl:template match="o[count(o[eo:void(.)])&gt;0 and count(o[not(eo:void(.))])&gt;0]" mode="with-attrs">
    <xsl:apply-templates select="." mode="with-diff-attrs"/>
  </xsl:template>
  <xsl:template match="o[o[not(eo:void(.))]/following-sibling::o[eo:void(.)]]" mode="with-diff-attrs">
    <xsl:element name="o">
      <xsl:apply-templates select="@*"/>
      <xsl:for-each select="o[eo:void(.)]">
        <xsl:copy-of select="."/>
      </xsl:for-each>
      <xsl:for-each select="o[not(eo:void(.))]">
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
