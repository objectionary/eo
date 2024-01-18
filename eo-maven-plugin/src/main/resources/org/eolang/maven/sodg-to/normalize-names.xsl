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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="normalize-names" version="2.0">
  <!--
  This one renames all <a/> elements that start with '$Φ.'. All
  vertices get simple unique integer-based numbers, such as 'v42'.
  -->
  <xsl:import href="/org/eolang/maven/sodg/_macros.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="sorted">
    <xsl:perform-sort select="/program/sodg/i/a[starts-with(text(), '$Φ.')]">
      <xsl:sort select="."/>
    </xsl:perform-sort>
  </xsl:variable>
  <xsl:function name="eo:renamed">
    <xsl:param name="v" as="xs:string"/>
    <xsl:text>$v</xsl:text>
    <xsl:value-of select="index-of(distinct-values($sorted/a), $v)"/>
  </xsl:function>
  <xsl:template match="/program/sodg/i/a[starts-with(text(), '$Φ.')]" priority="1">
    <xsl:copy>
      <xsl:value-of select="eo:renamed(.)"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="/program/sodg/i/a[text() = 'ν0']" priority="1">
    <xsl:copy>
      <xsl:text>v0</xsl:text>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
