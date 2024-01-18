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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="add-refs" version="2.0">
  <!--
  Here we go through all objects and find what their @base
  are referring to. If we find the object they refer to,
  we add a new @ref attribute to the object. Those objects
  which are not getting @ref attributes after this transformation
  are not visible in the current scope. Maybe they are
  global or just a mistake.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@base and not(starts-with(@base, '.')) and @base!='$' and @base!='^']">
    <xsl:variable name="current" select="."/>
    <xsl:copy>
      <xsl:variable name="parent" select="ancestor::*[o[@name=$current/@base]][1]"/>
      <xsl:if test="$parent">
        <xsl:variable name="source" select="$parent/o[@name=$current/@base]"/>
        <xsl:if test="$parent">
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
      </xsl:if>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
