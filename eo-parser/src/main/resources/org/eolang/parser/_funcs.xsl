<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2025 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:math="http://www.w3.org/2005/xpath-functions/math" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="_funcs" version="2.0">
  <xsl:function name="eo:has-data" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="normalize-space(string-join($o/text(), '')) != ''"/>
  </xsl:function>
  <xsl:function name="eo:abstract" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="not(exists($o/@base))"/>
  </xsl:function>
  <xsl:function name="eo:void" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="$o/@base='∅'"/>
  </xsl:function>
  <xsl:function name="eo:hex-to-utf8" as="xs:string">
    <xsl:param name="hex" as="xs:string"/>
    <xsl:variable name="hex-upper" select="upper-case(normalize-space($hex))"/>
    <xsl:variable name="length" select="string-length($hex-upper)"/>
    <xsl:variable name="hex-digits" select="string-to-codepoints('0123456789ABCDEF')"/>
    <xsl:variable name="decimal" select="sum(for $i in 1 to $length return (index-of($hex-digits, string-to-codepoints(substring($hex-upper, $i, 1))) - 1) * math:pow(16, $length - $i))"/>
    <xsl:value-of select="codepoints-to-string(xs:int($decimal))"/>
  </xsl:function>
  <xsl:function name="eo:bytes-to-string" as="xs:string">
    <xsl:param name="bytes" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="$bytes = '--'">
        <xsl:sequence select="''"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="byte-values" select="for $byte in tokenize($bytes, '-') return eo:hex-to-utf8($byte)"/>


        <xsl:sequence select="string-join(for $byte in tokenize($bytes, '-') return eo:hex-to-utf8($byte), '')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- Convert string bytes sequence to double number, e.g. 40-14-00-00-00-00-00-00 => 5 -->
  <xsl:function name="eo:bytes-to-number" as="xs:anyAtomicType">
    <xsl:param name="bytes"/>
    <xsl:variable name="hex" select="translate($bytes, '-', '')"/>
    <xsl:variable name="map" as="element()*">
      <entry h="0" b="0000"/>
      <entry h="1" b="0001"/>
      <entry h="2" b="0010"/>
      <entry h="3" b="0011"/>
      <entry h="4" b="0100"/>
      <entry h="5" b="0101"/>
      <entry h="6" b="0110"/>
      <entry h="7" b="0111"/>
      <entry h="8" b="1000"/>
      <entry h="9" b="1001"/>
      <entry h="A" b="1010"/>
      <entry h="B" b="1011"/>
      <entry h="C" b="1100"/>
      <entry h="D" b="1101"/>
      <entry h="E" b="1110"/>
      <entry h="F" b="1111"/>
    </xsl:variable>
    <xsl:variable name="bin" as="xs:string" select="string-join(for $c in string-to-codepoints(upper-case($hex)) return $map[@h = codepoints-to-string($c)]/@b, '')"/>
    <xsl:variable name="sign" select="if (substring($bin, 1, 1) = '1') then -1 else 1"/>
    <xsl:variable name="exponentBits" select="substring($bin, 2, 11)"/>
    <xsl:variable name="exponent" select="sum(for $i in 1 to string-length($exponentBits) return xs:double(substring($exponentBits, $i, 1)) * math:pow(2, string-length($exponentBits) - $i)) - 1023"/>
    <xsl:variable name="mantissaBits" select="substring($bin, 13, 52)"/>
    <xsl:variable name="mantissaValue">
      <xsl:sequence select="sum(for $i in 1 to string-length($mantissaBits) return xs:double(substring($mantissaBits, $i, 1)) * math:pow(2, -$i))"/>
    </xsl:variable>
    <xsl:sequence select="$sign * (1 + $mantissaValue) * math:pow(2, $exponent)"/>
  </xsl:function>
</xsl:stylesheet>
