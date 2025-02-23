<?xml version="1.0"?>
<!--
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:math="http://www.w3.org/2005/xpath-functions/math" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="_funcs" version="2.0">
  <xsl:import href="/org/eolang/parser/_specials.xsl"/>
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
    <xsl:sequence select="$o/@base=$eo:empty"/>
  </xsl:function>
  <!-- BYTES TO STRING -->
  <xsl:function name="eo:bytes-to-string" as="xs:string">
    <xsl:param name="bytes" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="$bytes = '--'">
        <xsl:sequence select="''"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="decoded">
          <xsl:for-each select="eo:decode-bytes(for $byte in (if (ends-with($bytes, '-')) then substring-before($bytes, '-') else tokenize($bytes, '-')) return eo:hex-to-utf8($byte))">
            <xsl:choose>
              <xsl:when test=".=10">
                <xsl:value-of select="'\n'"/>
              </xsl:when>
              <xsl:when test=".=9">
                <xsl:value-of select="'\t'"/>
              </xsl:when>
              <xsl:when test=".=13">
                <xsl:value-of select="'\r'"/>
              </xsl:when>
              <!-- Keep ASCII characters -->
              <xsl:when test=". ge 32 and . le 126">
                <xsl:variable name="char" select="codepoints-to-string(.)"/>
                <xsl:if test="$char='\' or $char='&quot;'">
                  <xsl:text>\</xsl:text>
                </xsl:if>
                <xsl:value-of select="$char"/>
              </xsl:when>
              <!-- Convert non-ASCII to \uXXXX -->
              <xsl:when test=". le 65535">
                <xsl:value-of select="concat('\u', eo:int-to-hex(xs:int(.)))"/>
              </xsl:when>
              <!-- Handle surrogate pairs for code points above U+FFFF -->
              <xsl:otherwise>
                <!-- 55296 = 0xD800 -->
                <xsl:variable name="cp1" select="xs:int(floor((. - 65536) div 1024) + 55296)"/>
                <!-- 56320 = 0xDC00 -->
                <xsl:variable name="cp2" select="xs:int(((. - 65536) mod 1024) + 56320)"/>
                <xsl:value-of select="concat('\u', eo:int-to-hex($cp1), '\u', eo:int-to-hex($cp2))"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
        </xsl:variable>
        <xsl:sequence select="$decoded"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- BYTES TO NUMBER, e.g. 40-14-00-00-00-00-00-00 => 5 -->
  <xsl:function name="eo:bytes-to-number" as="xs:anyAtomicType">
    <xsl:param name="bytes"/>
    <!-- Undash -->
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
    <!-- Sign bit (1 for negative, 0 for positive) -->
    <xsl:variable name="sign" select="if (substring($bin, 1, 1) = '1') then -1 else 1"/>
    <!-- Extract exponent (11 bits) and convert to integer -->
    <xsl:variable name="exponentBits" select="substring($bin, 2, 11)"/>
    <xsl:variable name="exponent" select="sum(for $i in 1 to string-length($exponentBits) return xs:double(substring($exponentBits, $i, 1)) * math:pow(2, string-length($exponentBits) - $i)) - 1023"/>
    <!-- Extract mantissa (52 bits) -->
    <xsl:variable name="mantissaBits" select="substring($bin, 13, 52)"/>
    <xsl:variable name="mantissaValue">
      <xsl:sequence select="sum(for $i in 1 to string-length($mantissaBits) return xs:double(substring($mantissaBits, $i, 1)) div math:pow(2, $i))"/>
    </xsl:variable>
    <!-- Compute final double value -->
    <xsl:sequence select="$sign * (1 + $mantissaValue) * math:pow(2, $exponent)"/>
  </xsl:function>
  <!-- HELPER FUNCTIONS -->
  <!-- Function to decode UTF-8 bytes into Unicode code points -->
  <xsl:function name="eo:decode-bytes" as="xs:integer*">
    <xsl:param name="bytes" as="xs:integer*"/>
    <xsl:choose>
      <!-- 1-byte sequence: 0xxxxxxx -->
      <xsl:when test="$bytes[1] lt 128">
        <xsl:sequence select="$bytes[1]"/>
        <xsl:sequence select="eo:decode-bytes(subsequence($bytes, 2))"/>
      </xsl:when>
      <!-- 2-byte sequence: 110xxxxx 10xxxxxx -->
      <xsl:when test="$bytes[1] ge 192 and $bytes[1] lt 224">
        <xsl:variable name="code-point" select="(($bytes[1] - 192) * 64) + ($bytes[2] - 128)"/>
        <xsl:sequence select="$code-point"/>
        <xsl:sequence select="eo:decode-bytes(subsequence($bytes, 3))"/>
      </xsl:when>
      <!-- 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx -->
      <xsl:when test="$bytes[1] ge 224 and $bytes[1] lt 240">
        <xsl:variable name="code-point" select="(($bytes[1] - 224) * 4096) + (($bytes[2] - 128) * 64) + ($bytes[3] - 128)"/>
        <xsl:sequence select="$code-point"/>
        <xsl:sequence select="eo:decode-bytes(subsequence($bytes, 4))"/>
      </xsl:when>
      <!-- 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx -->
      <xsl:when test="$bytes[1] ge 240 and $bytes[1] lt 248">
        <xsl:variable name="code-point" select="(($bytes[1] - 240) * 262144) + (($bytes[2] - 128) * 4096) + (($bytes[3] - 128) * 64) + ($bytes[4] - 128)"/>
        <xsl:sequence select="$code-point"/>
        <xsl:sequence select="eo:decode-bytes(subsequence($bytes, 5))"/>
      </xsl:when>
      <!-- Otherwise, return empty (should not occur if input is valid UTF-8) -->
      <xsl:otherwise/>
    </xsl:choose>
  </xsl:function>
  <!-- Function to convert integer to 4-digit hex string -->
  <xsl:function name="eo:int-to-hex" as="xs:string">
    <xsl:param name="value" as="xs:integer"/>
    <xsl:variable name="hex-chars" select="'0123456789ABCDEF'"/>
    <xsl:variable name="hex" select="concat(substring($hex-chars, floor($value idiv 4096) + 1, 1), substring($hex-chars, floor(($value mod 4096) idiv 256) + 1, 1), substring($hex-chars, floor(($value mod 256) idiv 16) + 1, 1), substring($hex-chars, ($value mod 16) + 1, 1))"/>
    <xsl:sequence select="$hex"/>
  </xsl:function>
  <xsl:function name="eo:hex-to-utf8" as="xs:integer">
    <xsl:param name="hex" as="xs:string"/>
    <xsl:variable name="hex-upper" select="upper-case(normalize-space($hex))"/>
    <xsl:variable name="length" select="string-length($hex-upper)"/>
    <xsl:variable name="hex-digits" select="string-to-codepoints('0123456789ABCDEF')"/>
    <xsl:variable name="decimal" select="sum(for $i in 1 to $length return (index-of($hex-digits, string-to-codepoints(substring($hex-upper, $i, 1))) - 1) * math:pow(16, $length - $i))"/>
    <xsl:value-of select="xs:int($decimal)"/>
  </xsl:function>
</xsl:stylesheet>
