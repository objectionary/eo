<?xml version="1.0"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2020 Yegor Bugayenko

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="rename-bases" version="2.0">
  <xsl:strip-space elements="*"/>
  <xsl:import href="/org/eolang/compiler/_funcs.xsl"/>
  <xsl:template match="o[@base and @ref]">
    <xsl:variable name="o" select="."/>
    <xsl:variable name="a" select="ancestor::o[eo:abstract(.)][1]"/>
    <xsl:if test="not($a)">
      <xsl:message terminate="yes">
        <xsl:text>Can't find ancestor of "</xsl:text>
        <xsl:value-of select="$o/@base"/>
        <xsl:text>" at line </xsl:text>
        <xsl:value-of select="$o/@ref"/>
      </xsl:message>
    </xsl:if>
    <xsl:variable name="v" select="$a/o[@line=$o/@ref and (starts-with(@name, concat($o/@base, '+')) or $o/@base = @name)]"/>
    <xsl:copy>
      <xsl:attribute name="base">
        <xsl:if test="$v">
          <xsl:value-of select="$v/@name"/>
        </xsl:if>
        <xsl:if test="not($v)">
          <xsl:value-of select="@base"/>
        </xsl:if>
      </xsl:attribute>
      <xsl:apply-templates select="node()|@* except @base"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
