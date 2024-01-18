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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="_macros" version="2.0">
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:function name="eo:index" as="xs:integer">
    <xsl:param name="o" as="node()"/>
    <xsl:variable name="ret">
      <xsl:choose>
        <xsl:when test="name($o) = 'o'">
          <xsl:value-of select="count($o/ancestor::o) + count($o/preceding::o) + 1"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>0</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:function name="eo:vertex" as="xs:string">
    <xsl:param name="o" as="node()"/>
    <xsl:variable name="ret">
      <xsl:text>vertex:</xsl:text>
      <xsl:value-of select="eo:index($o)"/>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:function name="eo:base-to-loc" as="xs:string">
    <xsl:param name="o" as="node()"/>
    <xsl:variable name="program" select="$o/ancestor::objects/ancestor::program"/>
    <xsl:variable name="ret">
      <xsl:choose>
        <xsl:when test="starts-with($o/@base, '.')">
          <xsl:value-of select="eo:base-to-loc($o/o[1])"/>
          <xsl:value-of select="$o/@base"/>
        </xsl:when>
        <xsl:when test="$o/@ref">
          <xsl:value-of select="$o//ancestor::objects//o[@name=$o/@base and @line=$o/@ref]/@loc"/>
        </xsl:when>
        <xsl:when test="$o/@base = 'Q'">
          <xsl:text>Φ</xsl:text>
        </xsl:when>
        <xsl:when test="$o/@base = '$'">
          <xsl:value-of select="$o/ancestor::o[@abstract][1]/@loc"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>Φ.</xsl:text>
          <xsl:if test="not(contains($o/@base, '.')) and $program/metas/meta[head='package']">
            <xsl:value-of select="$program/metas/meta[head='package']/tail"/>
            <xsl:text>.</xsl:text>
          </xsl:if>
          <xsl:value-of select="$o/@base"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:function name="eo:parent-of-loc" as="xs:string">
    <xsl:param name="loc" as="xs:string"/>
    <xsl:variable name="ret">
      <xsl:variable name="parts" select="tokenize($loc, '\.')"/>
      <xsl:for-each select="$parts">
        <xsl:if test="position() != last()">
          <xsl:if test="position() &gt; 1">
            <xsl:text>.</xsl:text>
          </xsl:if>
          <xsl:value-of select="."/>
        </xsl:if>
      </xsl:for-each>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:function name="eo:var" as="xs:string">
    <xsl:param name="v" as="xs:string"/>
    <xsl:variable name="ret">
      <xsl:choose>
        <xsl:when test="$v = 'Φ'">
          <xsl:text>ν0</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>$</xsl:text>
          <xsl:value-of select="$v"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:function name="eo:alpha" as="xs:string">
    <xsl:param name="o" as="node()"/>
    <xsl:variable name="ret">
      <xsl:choose>
        <xsl:when test="$o/@name">
          <xsl:value-of select="$o/@name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="order" select="count($o/preceding-sibling::o)"/>
          <xsl:choose>
            <xsl:when test="starts-with($o/ancestor::*[1]/@base, '.')">
              <xsl:choose>
                <xsl:when test="$order = 0">
                  <xsl:text>ρ</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:text>α</xsl:text>
                  <xsl:value-of select="$order - 1"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>α</xsl:text>
              <xsl:value-of select="$order"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:function name="eo:attr" as="xs:string">
    <xsl:param name="a" as="xs:string"/>
    <xsl:variable name="ret">
      <xsl:choose>
        <xsl:when test="$a = '@'">
          <xsl:text>φ</xsl:text>
        </xsl:when>
        <xsl:when test="$a = 'Q'">
          <xsl:text>Φ</xsl:text>
        </xsl:when>
        <xsl:when test="$a = '^'">
          <xsl:text>ρ</xsl:text>
        </xsl:when>
        <xsl:when test="$a = '&amp;'">
          <xsl:text>σ</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$a"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:template name="i">
    <xsl:param name="name" as="xs:string"/>
    <xsl:param name="args" as="item()*"/>
    <xsl:param name="comment" as="xs:string" select="''"/>
    <xsl:element name="i">
      <xsl:attribute name="name">
        <xsl:value-of select="$name"/>
      </xsl:attribute>
      <xsl:for-each select="$args">
        <xsl:element name="a">
          <xsl:value-of select="."/>
        </xsl:element>
      </xsl:for-each>
      <xsl:element name="c">
        <xsl:value-of select="$comment"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>
</xsl:stylesheet>
