<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2022 Objectionary.com

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
<!--
 @todo #1104:30m This code tests for both org.eolang.TYPE and TYPE.
  This is unnecessary. Investigate why add-default-package.xsl
  is not always called from eo-maven-plugin and use
  only qualified object names.
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
          <xsl:choose>
            <xsl:when test="@data='bytes'">
              <xsl:variable name="array">
                <xsl:text>new byte[] {</xsl:text>
                <xsl:for-each select="tokenize(text(), ' ')">
                  <xsl:if test="position() &gt; 1">
                    <xsl:text>, </xsl:text>
                  </xsl:if>
                  <xsl:text>(byte) 0x</xsl:text>
                  <xsl:value-of select="."/>
                </xsl:for-each>
                <xsl:text>}</xsl:text>
              </xsl:variable>
              <xsl:choose>
                <xsl:when test="@base='org.eolang.string' or @base='string'">
                  <xsl:text>new String(</xsl:text>
                  <xsl:value-of select="$array"/>
                  <xsl:text>, java.nio.charset.StandardCharsets.UTF_8</xsl:text>
                  <xsl:text>)</xsl:text>
                </xsl:when>
                <xsl:when test="@base='org.eolang.bool' or @base='bool'">
                  <xsl:choose>
                    <xsl:when test="text() = '01'">
                      <xsl:text>Boolean.TRUE</xsl:text>
                    </xsl:when>
                    <xsl:otherwise>
                      <xsl:text>Boolean.FALSE</xsl:text>
                    </xsl:otherwise>
                  </xsl:choose>
                </xsl:when>
                <xsl:when test="@base='org.eolang.bytes' or @base='bytes'">
                  <xsl:value-of select="$array"/>
                </xsl:when>
                <xsl:when test="@base='org.eolang.int' or @base='int'">
                  <xsl:text>java.nio.ByteBuffer.wrap(</xsl:text>
                  <xsl:value-of select="$array"/>
                  <xsl:text>).getLong()</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:text>/** ERROR: Unsupported type: </xsl:text>
                  <xsl:value-of select="@base"/>
                  <xsl:text>**/</xsl:text>
                </xsl:otherwise>
              </xsl:choose>
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
