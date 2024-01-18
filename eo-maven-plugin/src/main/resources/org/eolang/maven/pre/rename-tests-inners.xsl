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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="rename-tests-inners" version="2.0">
  <!--
  This stylesheet renames inner classes, which were placed
  into tests objects by tests.xsl. Without this cleaning there
  could be too long class names.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="tests" select="exists(//meta[head='junit' or head='tests'])"/>
  <xsl:function name="eo:name-of" as="xs:string">
    <xsl:param name="class" as="node()"/>
    <xsl:variable name="ret">
      <xsl:choose>
        <xsl:when test="$tests and $class/@parent">
          <xsl:variable name="parent" select="$class/@parent"/>
          <xsl:text>ω</xsl:text>
          <xsl:value-of select="$class/@ancestors"/>
          <xsl:value-of select="substring-after($class/@name, concat($class/@parent, '$'))"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$class/@name"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$ret"/>
  </xsl:function>
  <xsl:template match="class">
    <xsl:copy>
      <xsl:if test="@name">
        <xsl:attribute name="name">
          <xsl:value-of select="eo:name-of(.)"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@parent">
        <xsl:attribute name="parent">
          <xsl:variable name="class" select="."/>
          <xsl:variable name="parent" select="//class[@name=$class/@parent]"/>
          <xsl:value-of select="eo:name-of($parent)"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="@* except (@name|@parent)"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[@base]">
    <xsl:copy>
      <xsl:attribute name="base">
        <xsl:variable name="o" select="."/>
        <xsl:variable name="class" select="//class[@name=$o/@base and @line=$o/@ref]"/>
        <xsl:choose>
          <xsl:when test="$class">
            <xsl:value-of select="eo:name-of($class)"/>
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
