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
<!--
  @todo #1115:30m Add conversions for other types (
    int, float, bytes, etc.) and values here.
    When all simple cases are covered, add support
    for recursive reduction.
    Such as 01-.as-bool.as-bytes.as-bool.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="constant-folding" version="2.0">
  <!--
  Fold expressions like FF-FF.as-int to actual integer value,
  and remove method call.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@base='.as-bool' and child::o[@base='org.eolang.bytes' and not(*)]]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:variable name="c" select="child::o"/>
      <xsl:attribute name="base">org.eolang.bool</xsl:attribute>
      <xsl:element name="o">
        <xsl:attribute name="base">org.eolang.bytes</xsl:attribute>
        <xsl:attribute name="data">bytes</xsl:attribute>
        <xsl:value-of select="$c[text()]"/>
      </xsl:element>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
