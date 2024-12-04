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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="const-to-dataized" version="2.0">
  <!--
  Replace @const with dataized.as-bytes
  1. a > b!     => (dataized a).as-bytes > b
  2. x.y z > m! => (dataized (x.y z)).as-bytes > m
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- Generate unique name for an abstract object -->
  <xsl:function name="eo:unique-name">
    <xsl:param name="name"/>
    <xsl:param name="scope"/>
    <xsl:param name="counter"/>
    <xsl:variable name="unique" select="concat($name, '-', $counter)"/>
    <xsl:choose>
      <xsl:when test="$scope[o[@name=$unique]]">
        <xsl:value-of select="eo:unique-name($name, $scope, $counter + 1)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$unique"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- Template -->
  <xsl:template match="o[@const]">
    <xsl:element name="o">
      <xsl:attribute name="base" select="'.as-bytes'"/>
      <xsl:attribute name="name" select="@name"/>
      <xsl:attribute name="line" select="@line"/>
      <xsl:attribute name="pos" select="@pos + 8"/>
      <xsl:element name="o">
        <xsl:attribute name="base" select="'org.eolang.dataized'"/>
        <xsl:element name="o">
          <xsl:for-each select="@*[name()!='const' and name()!='name']">
            <xsl:attribute name="{name()}">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:for-each>
          <xsl:if test="eo:abstract(.)">
            <xsl:attribute name="name">
              <xsl:value-of select="eo:unique-name(@name, ./parent::o, 1)"/>
            </xsl:attribute>
          </xsl:if>
          <xsl:for-each select="o">
            <xsl:apply-templates select="."/>
          </xsl:for-each>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
