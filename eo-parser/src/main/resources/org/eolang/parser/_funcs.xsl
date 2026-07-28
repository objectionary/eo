<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:math="http://www.w3.org/2005/xpath-functions/math" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="_funcs" version="2.0">
  <xsl:import href="/org/eolang/parser/_specials.xsl"/>
  <xsl:function name="eo:has-data" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="normalize-space(string-join($o/text(), '')) != ''"/>
  </xsl:function>
  <xsl:function name="eo:read-data" as="xs:string">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="replace(string-join($o/text(),''), '^\s+|\s+$', '')"/>
  </xsl:function>
  <xsl:function name="eo:abstract" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="not(exists($o/@base))"/>
  </xsl:function>
  <xsl:function name="eo:void" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="$o/@base=$eo:empty"/>
  </xsl:function>
  <xsl:function name="eo:atom" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="exists($o/o[@name=$eo:lambda])"/>
  </xsl:function>
  <xsl:function name="eo:test-attr" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="starts-with($o/@name, '+') or starts-with($o/@name, '-')"/>
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
  <!-- Escape `+` in test name syntax. -->
  <xsl:function name="eo:escape-plus" as="xs:string">
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="if (contains($name, '+')) then concat(substring-before($name, '+'), substring-after($name, '+')) else $name"/>
  </xsl:function>
</xsl:stylesheet>
