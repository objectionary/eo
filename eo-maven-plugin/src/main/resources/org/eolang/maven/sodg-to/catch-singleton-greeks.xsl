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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="catch-singleton-greeks" version="2.0">
  <!--
  Here we catch vertices that have "epsilon" departing edges and
  some other edges in addition to them. We don't want this to happen,
  since "epsilon" must always be alone.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="greeks" select="('ε', 'γ')"/>
  <xsl:template match="/graph/v">
    <xsl:variable name="v" select="."/>
    <xsl:for-each select="$greeks">
      <xsl:variable name="letter" select="."/>
      <xsl:if test="$v/e[@title=$letter] and $v/e[@title!=$letter]">
        <xsl:message terminate="yes">
          <xsl:text>The edge departing from '</xsl:text>
          <xsl:value-of select="$v/@id"/>
          <xsl:text>' must be alone, since it's labeled as '</xsl:text>
          <xsl:value-of select="$letter"/>
          <xsl:text>'</xsl:text>
        </xsl:message>
      </xsl:if>
    </xsl:for-each>
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
