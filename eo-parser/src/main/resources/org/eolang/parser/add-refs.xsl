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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="http://eolang.com" id="add-refs" version="2.0">
  <!--
  Here we go through all objects and find what their @base
  are referring to. If we find the object they refer to,
  we add a new @ref attribute to the object. Those objects
  which are not getting @ref attributes after this transformation
  are not visible in the current scope. Maybe they are
  global or just a mistake.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:key name="o-by-name" match="o[@name]" use="@name"/>
  <xsl:template match="o[@base]">
    <xsl:apply-templates select="." mode="with-base"/>
  </xsl:template>
  <xsl:template match="o[not(starts-with(@base, '.'))]" mode="with-base">
    <xsl:apply-templates select="." mode="no-dots"/>
  </xsl:template>
  <xsl:template match="o[@base!='$' and @base!='^']" mode="no-dots">
    <xsl:variable name="current" select="."/>
    <xsl:variable name="source" select="eo:closest($current, key('o-by-name', @base))"/>
    <xsl:copy>
      <xsl:if test="$source">
        <xsl:if test="count($source)!=1">
          <xsl:message terminate="yes">
            <xsl:text>Duplicate names inside "</xsl:text>
            <xsl:value-of select="@name"/>
            <xsl:text>", the base is "</xsl:text>
            <xsl:value-of select="@base"/>
            <xsl:text>" at the line #</xsl:text>
            <xsl:value-of select="@line"/>
            <xsl:text> pointing to </xsl:text>
            <xsl:for-each select="$source">
              <xsl:if test="position()&gt;1">
                <xsl:text>, </xsl:text>
              </xsl:if>
              <xsl:text>&lt;</xsl:text>
              <xsl:value-of select="name(.)"/>
              <xsl:text>/&gt;</xsl:text>
              <xsl:text> at line #</xsl:text>
              <xsl:value-of select="@line"/>
            </xsl:for-each>
            <xsl:text>; it's internal bug</xsl:text>
          </xsl:message>
        </xsl:if>
        <xsl:if test="not($source/@line)">
          <xsl:message terminate="yes">
            <xsl:text>Attribute @line is absent at "</xsl:text>
            <xsl:value-of select="$source/@name"/>
            <xsl:text>"</xsl:text>
          </xsl:message>
        </xsl:if>
        <xsl:attribute name="ref">
          <xsl:value-of select="$source/@line"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <xsl:function name="eo:closest" as="node()?">
    <xsl:param name="current-node" as="node()"/>
    <xsl:param name="nodes" as="node()*"/>
    <xsl:variable name="intersecting-nodes" select="$nodes[eo:has-intersecting-route($current-node, .)]"/>
    <xsl:for-each select="$intersecting-nodes">
      <xsl:sort select="string-length(eo:get-route(.))" order="descending" data-type="number"/>
      <xsl:if test="position() = 1">
        <xsl:copy-of select="."/>
      </xsl:if>
    </xsl:for-each>
  </xsl:function>
  <xsl:function name="eo:has-intersecting-route" as="xs:boolean">
    <xsl:param name="node1" as="node()"/>
    <xsl:param name="node2" as="node()"/>
    <xsl:variable name="route1" select="eo:get-route($node1)"/>
    <xsl:variable name="route2" select="eo:get-route($node2)"/>
    <xsl:sequence select="starts-with($route1, $route2)"/>
  </xsl:function>
  <xsl:function name="eo:get-route" as="xs:string">
    <xsl:param name="node" as="node()"/>
    <xsl:variable name="ancestors" select="$node/ancestor::*"/>
    <xsl:sequence select="string-join(for $ancestor in $ancestors return generate-id($ancestor), '/')"/>
  </xsl:function>
</xsl:stylesheet>
