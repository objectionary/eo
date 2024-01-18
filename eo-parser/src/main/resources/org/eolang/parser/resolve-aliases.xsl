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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="resolve-aliases" version="2.0">
  <!--
  Here we go through all objects that DON'T have @ref attributes
  and try to find their references in aliases. If we find them,
  we change their @base attributes. If not, we decide that they
  are in org.eolang package and also change the @base attribute.

  If some alias is badly formatted, a runtime error is issued.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[not(@ref) and @base and not(contains(@base, '.'))]">
    <xsl:variable name="o" select="."/>
    <xsl:copy>
      <xsl:attribute name="base">
        <xsl:variable name="meta" select="/program/metas/meta[head='alias' and part[1] = $o/@base]"/>
        <xsl:choose>
          <xsl:when test="$meta">
            <xsl:variable name="tail" select="$meta/part[2]"/>
            <xsl:if test="$tail = ''">
              <xsl:message terminate="yes">
                <xsl:text>The alias "</xsl:text>
                <xsl:value-of select="$meta/head"/>
                <xsl:text>" doesn't have the tail part: "</xsl:text>
                <xsl:value-of select="$meta/tail"/>
                <xsl:text>" at line </xsl:text>
                <xsl:value-of select="$meta/@line"/>
              </xsl:message>
            </xsl:if>
            <xsl:value-of select="$tail[1]"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$o/@base"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates select="node()|@* except @base"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
