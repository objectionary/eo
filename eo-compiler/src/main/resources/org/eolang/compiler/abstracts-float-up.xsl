<?xml version="1.0"?>
<!--
The MIT License (MIT)

Copyright (c) 2017-2019 Yegor Bugayenko

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xsl:strip-space elements="*"/>
  <xsl:function name="eo:abstract" as="xs:boolean">
    <xsl:param name="object" as="element()"/>
    <xsl:sequence select="not(exists($object/@base)) and exists($object/o)"/>
  </xsl:function>
  <xsl:template match="o[eo:abstract(.)]">
    <xsl:element name="o">
      <xsl:attribute name="base">
        <xsl:for-each select="ancestor::o[eo:abstract(.)]">
          <xsl:value-of select="@name"/>
          <xsl:text>$</xsl:text>
        </xsl:for-each>
        <xsl:value-of select="@name"/>
      </xsl:attribute>
      <xsl:attribute name="name">
        <xsl:value-of select="@name"/>
      </xsl:attribute>
      <xsl:attribute name="line">
        <xsl:value-of select="@line"/>
      </xsl:attribute>
      <xsl:for-each select="ancestor::o[eo:abstract(.)]">
        <xsl:sort data-type="number" select="position()" order="descending"/>
        <xsl:variable name="level" select="position()"/>
        <xsl:for-each select="./o[not(@base) and @name and not(./o)]">
          <xsl:element name="o">
            <xsl:attribute name="name">
              <xsl:value-of select="@name"/>
              <xsl:for-each select="1 to $level">
                <xsl:text>+</xsl:text>
              </xsl:for-each>
            </xsl:attribute>
            <xsl:attribute name="ref">
              <xsl:value-of select="@line"/>
            </xsl:attribute>
            <xsl:attribute name="base">
              <xsl:value-of select="@name"/>
              <xsl:for-each select="1 to ($level - 1)">
                <xsl:text>+</xsl:text>
              </xsl:for-each>
            </xsl:attribute>
          </xsl:element>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>
  <xsl:template match="o[eo:abstract(.)]" mode="top">
    <xsl:variable name="o" select="."/>
    <xsl:copy>
      <xsl:attribute name="name">
        <xsl:for-each select="ancestor-or-self::o[eo:abstract(.)]">
          <xsl:value-of select="@name"/>
          <xsl:if test="position() != last()">
            <xsl:text>$</xsl:text>
          </xsl:if>
        </xsl:for-each>
      </xsl:attribute>
      <xsl:apply-templates select="node()|@* except @name"/>
      <xsl:for-each select="ancestor::o[eo:abstract(.)]">
        <xsl:sort data-type="number" select="position()" order="descending"/>
        <xsl:variable name="level" select="position()"/>
        <xsl:for-each select="./o[not(@base) and @name and not(./o) and generate-id(.)!=generate-id($o)]">
          <xsl:element name="o">
            <xsl:attribute name="name">
              <xsl:value-of select="@name"/>
              <xsl:for-each select="1 to $level">
                <xsl:text>+</xsl:text>
              </xsl:for-each>
            </xsl:attribute>
            <xsl:attribute name="line">
              <xsl:value-of select="@line"/>
            </xsl:attribute>
          </xsl:element>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="//objects">
    <xsl:copy>
      <xsl:apply-templates select=".//o[eo:abstract(.)]" mode="top"/>
      <xsl:apply-templates select="node()[not(eo:abstract(.))]|@*"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
