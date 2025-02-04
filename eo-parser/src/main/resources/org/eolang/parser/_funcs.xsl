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
  <xsl:function name="eo:hex-to-utf8" as="xs:anyAtomicType">
    <xsl:param name="hex" as="xs:string"/>
    <xsl:variable name="hex-upper" select="upper-case(normalize-space($hex))"/>
    <xsl:variable name="hex-digits" select="string-to-codepoints('0123456789ABCDEF')"/>
    <xsl:variable name="decimal" select="
      sum(
        for $i in 1 to string-length($hex-upper)
          return
            (index-of($hex-digits, string-to-codepoints(substring($hex-upper, $i, 1))) - 1)
              * math:pow(16, string-length($hex-upper) - $i)
      )
    "/>
    <xsl:choose>
      <xsl:when test="$decimal &gt;= 32 and $decimal &lt;= 126">
        <xsl:sequence select="codepoints-to-string(xs:int($decimal))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="$decimal"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="eo:unhex" as="xs:anyAtomicType">
    <xsl:param name="bytes" as="xs:string"/>
    <xsl:sequence select="
      string-join(
        for $byte in tokenize($bytes, '-')
          return eo:hex-to-utf8($byte)
        , ''
      )"
    />
  </xsl:function>
</xsl:stylesheet>
