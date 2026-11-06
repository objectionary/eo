<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="data" version="3.0">
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  The longest data written as an array literal. Java compiles every element
  of such an array to about seven bytes of bytecode, and a method can hold
  no more than 64KB of it, so a longer one goes into string constants and is
  turned back into bytes at runtime (#9344).
  -->
  <xsl:variable name="inline" select="256"/>
  <!--
  How many bytes go into one string constant. A constant takes at most two
  bytes of the class file per byte of data, and must fit into 65535 of them.
  -->
  <xsl:variable name="chunk" select="1024"/>
  <!-- One byte of data, written as a character of a Java string literal. -->
  <xsl:function name="eo:java-char" as="xs:string">
    <xsl:param name="octet" as="xs:string"/>
    <xsl:variable name="digits" select="'0123456789ABCDEF'"/>
    <xsl:variable name="code" select="string-length(substring-before($digits, upper-case(substring($octet, 1, 1)))) * 16 + string-length(substring-before($digits, upper-case(substring($octet, 2, 1))))"/>
    <xsl:choose>
      <xsl:when test="$code = (34, 92)">
        <xsl:sequence select="concat('\', codepoints-to-string($code))"/>
      </xsl:when>
      <xsl:when test="$code ge 32 and $code le 126">
        <xsl:sequence select="codepoints-to-string($code)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="concat('\', $code idiv 64, $code idiv 8 mod 8, $code mod 8)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:template match="o[o[1][eo:has-data(.)]]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:variable name="octets" select="tokenize(o[1]/text(), '-')[. != '']"/>
      <value>
        <xsl:choose>
          <xsl:when test="count($octets) &gt; $inline">
            <xsl:text>String.join("", </xsl:text>
            <xsl:for-each select="0 to (count($octets) - 1) idiv $chunk">
              <xsl:if test=". &gt; 0">
                <xsl:text>, </xsl:text>
              </xsl:if>
              <xsl:text>"</xsl:text>
              <xsl:value-of select="string-join(subsequence($octets, . * $chunk + 1, $chunk) ! eo:java-char(.), '')"/>
              <xsl:text>"</xsl:text>
            </xsl:for-each>
            <xsl:text>).getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>new byte[] {</xsl:text>
            <xsl:for-each select="$octets">
              <xsl:if test="position() &gt; 1">
                <xsl:text>, </xsl:text>
              </xsl:if>
              <xsl:text>(byte) 0x</xsl:text>
              <xsl:value-of select="."/>
            </xsl:for-each>
            <xsl:text>}</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </value>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
