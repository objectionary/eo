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
  - QQ.bool TRUE > TRUE

  In the level of xmir it looks like:
  - <o base="org.eolang.int" data="int">2</o>    =>  <o base="org.eolang.int" data="int">2</o>
  - <o base="org.eolang.int" name="num">             <o base="org.eolang.int" data="int" name="num">
      <o base="org.eolang.int" data="int">       =>    42
        42                                           </o>
      </o>
    </o>
  - <o base=".bool" name="b">                        <o base="org.eolang.bool" data="bytes" name="b">
      <o base=".eolang">                         =>    01
        <o base=".org">                              </o>
          <o base="Q"></o>
        </o>
      </o
      <o base="org.eolang.bool" data="bytes">
        01
      </o>
    </o>
  -->
  <xsl:import href="/org/eolang/parser/_datas.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="reversed" as="element()*">
    <xsl:for-each select="$literal-objects">
      <a>
        <xsl:value-of select="replace(./text(), 'org\.eolang\.', '.')"/>
      </a>
    </xsl:for-each>
  </xsl:variable>
  <xsl:template match="//o[o[last() and @data] and @base!='org.eolang.tuple']">
    <xsl:variable name="base" select="@base"/>
    <xsl:variable name="last-base" select="o[last()]/@base"/>
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
      <xsl:when test="$reversed[text()=$base] and count(o)=2 and o[position()=1 and @base='.eolang' and o[@base='.org' and o[@base='Q']]] and o[last() and @data] and $literal-objects[text()=$last-base]">
        <o>
          <xsl:for-each select="@*[name()!='base']">
            <xsl:attribute name="{name()}">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:for-each>
          <xsl:attribute name="data">
            <xsl:value-of select="o[last()]/@data"/>
          </xsl:attribute>
          <xsl:attribute name="base">
            <xsl:value-of select="o[last()]/@base"/>
          </xsl:attribute>
          <xsl:value-of select="o[last()]"/>
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
