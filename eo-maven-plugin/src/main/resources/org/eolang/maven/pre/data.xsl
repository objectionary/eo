<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2022 Yegor Bugayenko

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="pre-data" version="2.0">
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@data]">
    <xsl:variable name="o" select="."/>
    <xsl:copy>
      <xsl:apply-templates select="@* except @data"/>
      <xsl:if test="@data='array'">
        <xsl:element name="array">
          <xsl:apply-templates select="node()"/>
        </xsl:element>
      </xsl:if>
      <xsl:if test="@data!='array'">
        <xsl:attribute name="primitive">
          <xsl:value-of select="@data"/>
        </xsl:attribute>
        <xsl:element name="value">
          <xsl:attribute name="java-type">
            <xsl:choose>
              <xsl:when test="@data='bytes'">
                <xsl:text>byte[]</xsl:text>
              </xsl:when>
              <xsl:when test="@data='string'">
                <xsl:text>String</xsl:text>
              </xsl:when>
              <xsl:when test="@data='float'">
                <xsl:text>Double</xsl:text>
              </xsl:when>
              <xsl:when test="@data='int'">
                <xsl:text>Long</xsl:text>
              </xsl:when>
              <xsl:when test="@data='bool'">
                <xsl:text>Boolean</xsl:text>
              </xsl:when>
              <xsl:otherwise>
                <xsl:message terminate="yes">
                  <xsl:text>Unknown data type "</xsl:text>
                  <xsl:value-of select="@data"/>
                  <xsl:text>"</xsl:text>
                </xsl:message>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
          <xsl:choose>
            <xsl:when test="@data='bytes'">
              <xsl:text>new byte[] {</xsl:text>
              <xsl:for-each select="tokenize(text(), ' ')">
                <xsl:if test="position() &gt; 1">
                  <xsl:text>, </xsl:text>
                </xsl:if>
                <xsl:text>(byte) 0x</xsl:text>
                <xsl:value-of select="."/>
              </xsl:for-each>
              <xsl:text>}</xsl:text>
            </xsl:when>
            <xsl:when test="@data='string'">
              <xsl:text>"</xsl:text>
              <xsl:value-of select="text()"/>
              <xsl:text>"</xsl:text>
            </xsl:when>
            <xsl:when test="@data='int'">
              <xsl:value-of select="text()"/>
              <xsl:text>L</xsl:text>
            </xsl:when>
            <xsl:when test="@data='float'">
              <xsl:value-of select="text()"/>
              <xsl:text>d</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="text()"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:element>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
