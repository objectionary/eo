<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2025 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="resolve-before-star" version="2.0">
  <!--
    Converts such XMIR with @before-star attributes:

    <o base="seq" before-star="2">
      <o base="1".../>
      <o base="2".../>
      <o base="3".../>
      <o base="4".../>
    </o>

    Into the next one:

    <o base="seq">
      <o base="1".../>
      <o base="2".../>
      <o base="tuple" star="">
        <o base="3".../>
        <o base="4".../>
      </o>
    </o>
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@before-star]">
    <xsl:variable name="before" select="@before-star" as="xs:int"/>
    <xsl:element name="o">
      <xsl:for-each select="@* except @before-star">
        <xsl:attribute name="{name()}" select="."/>
      </xsl:for-each>
      <xsl:for-each select="o[position() &lt;= $before]">
        <xsl:apply-templates select="."/>
      </xsl:for-each>
      <xsl:element name="o">
        <xsl:attribute name="base" select="'tuple'"/>
        <xsl:attribute name="star"/>
        <xsl:for-each select="o[position() &gt; $before]">
          <xsl:apply-templates select="."/>
        </xsl:for-each>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
