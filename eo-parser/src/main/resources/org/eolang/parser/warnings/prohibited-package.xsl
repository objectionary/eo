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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="prohibited-package" version="2.0">
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="white-list">
    <a>as-phi</a>
    <a>bool</a>
    <a>bytes</a>
    <a>cage</a>
    <a>cti</a>
    <a>error</a>
    <a>float</a>
    <a>goto</a>
    <a>heap</a>
    <a>int</a>
    <a>memory</a>
    <a>nan</a>
    <a>negative-infinity</a>
    <a>nop</a>
    <a>positive-infinity</a>
    <a>ram</a>
    <a>rust</a>
    <a>seq</a>
    <a>string</a>
    <a>switch</a>
    <a>try</a>
    <a>tuple</a>
  </xsl:variable>
  <xsl:variable name="name" select="//objects/o[1]/@name"/>
  <xsl:template match="/program/errors">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:for-each select="/program/metas/meta">
        <xsl:variable name="meta-head" select="head"/>
        <xsl:variable name="meta-tail" select="tail"/>
        <xsl:if test="$meta-head='package' and $meta-tail='org.eolang' and not($white-list/a=$name)">
          <xsl:element name="error">
            <xsl:attribute name="check">
              <xsl:text>prohibited-package</xsl:text>
            </xsl:attribute>
            <xsl:attribute name="line">
              <xsl:value-of select="@line"/>
            </xsl:attribute>
            <xsl:attribute name="severity">
              <xsl:text>warning</xsl:text>
            </xsl:attribute>
            <xsl:text>Package org.eolang is reserved for internal object only</xsl:text>
          </xsl:element>
        </xsl:if>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
