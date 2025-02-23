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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="https://www.eolang.org" id="unhex-data" version="2.0">
  <!--
    Performs the reverse operation of "/org/eolang/parser/stars-to-tuples.xsl"
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:template match="o[@base='Q.org.eolang.string' and o[1][eo:has-data(.)]]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:text>"</xsl:text>
      <xsl:value-of select="eo:bytes-to-string(o[1]/text())"/>
      <xsl:text>"</xsl:text>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[@base='Q.org.eolang.number' and o[1][eo:has-data(.)]]">
    <xsl:variable name="bytes" select="o[1]/text()"/>
    <xsl:choose>
      <xsl:when test="$bytes='7F-F8-00-00-00-00-00-00' or $bytes='7F-F0-00-00-00-00-00-00' or $bytes='FF-F0-00-00-00-00-00-00' or $bytes='FF-FF-FF-FF-FF-FF-FF-FF'">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test="$bytes='00-00-00-00-00-00-00-00'">
        <xsl:copy>
          <xsl:apply-templates select="@*"/>
          <xsl:text>0</xsl:text>
        </xsl:copy>
      </xsl:when>
      <xsl:when test="$bytes='80-00-00-00-00-00-00-00'">
        <xsl:copy>
          <xsl:apply-templates select="@*"/>
          <xsl:text>-0</xsl:text>
        </xsl:copy>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="@*"/>
          <xsl:value-of select="translate(xs:string(eo:bytes-to-number($bytes)), 'E', 'e')"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
