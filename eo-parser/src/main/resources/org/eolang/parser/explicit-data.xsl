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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="explicit-data" version="2.0">
  <!--
  Here we transform just data into application
  - 5 => 5
  - int 5 => 5
  - float 22.4 => 22.4

  In the level of xmir it looks like:
  - <o base="int" data="int">2</o> => <o base="int" data="int">2</o>
  - <o base="int" name="num">        <o base="int" data="int" name="num">
      <o base="int" data="int">  =>    42
        42                           </o>
      </o>
    </o>
  -->
  <xsl:import href="/org/eolang/parser/_datas.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="//o[o[@data and @base=../@base and @base!='org.eolang.tuple']]">
    <xsl:variable name="base" select="@base"/>
    <xsl:choose>
      <xsl:when test="$literal-objects[text()=$base]">
        <o>
          <xsl:for-each select="@*">
            <xsl:attribute name="{name()}">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:for-each>
          <xsl:attribute name="data">
            <xsl:value-of select="o/@data"/>
          </xsl:attribute>
          <xsl:value-of select="o"/>
        </o>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
