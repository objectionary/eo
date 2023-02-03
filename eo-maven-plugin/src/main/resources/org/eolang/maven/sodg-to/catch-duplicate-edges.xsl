<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2023 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="catch-duplicate-edges" version="2.0">
  <!--
  Here we go through all vertices and confirm that they don't have
  duplicate edges (with the same label).
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/graph/v/e[preceding-sibling::e/@title = @title]">
    <xsl:variable name="e" select="."/>
    <xsl:message terminate="yes">
      <xsl:text>The edge </xsl:text>
      <xsl:value-of select="$e/@id"/>
      <xsl:text> labeled as '</xsl:text>
      <xsl:value-of select="$e/@title"/>
      <xsl:text>' is a duplicate of another edge with the same label</xsl:text>
    </xsl:message>
  </xsl:template>
  <xsl:template match="/graph/v/e[preceding-sibling::e/@id = @id]">
    <xsl:variable name="e" select="."/>
    <xsl:message terminate="yes">
      <xsl:text>The edge </xsl:text>
      <xsl:value-of select="$e/@id"/>
      <xsl:text>' is a duplicate of another edge with the same @id</xsl:text>
    </xsl:message>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
