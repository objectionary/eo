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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="explicit-data" version="2.0">
  <!--
  Here we transform just data into application
  - 5             => int 5
  - int 5         => int 5
  - 42.2          => float 42.2
  - float 22.4    => float 22.4
  - TRUE          => bool TRUE
  - bool TRUE     => bool TRUE
  - "Hey"         => string "Hey"
  - QQ.string "H" => QQ.string "H"

  In the level of xmir it looks like:
  - <o base="org.eolang.bytes" data="bytes">         <o base="org.eolang.bytes" data="bytes">
      22-32                                      =>    22-32
    </o>                                             </o>
  - <o base="org.eolang.int" name="num">             <o base="org.eolang.int" name="num">
      <o base="org.eolang.int" data="bytes">           <o base=org.eolang.bytes data="bytes">
        00 00 00 00 00 00 00 01                  =>      00 00 00 00 00 00 00 01
      </o>                                             </o>
    </o>                                             </o>
  - <o base=".bool" name="b">                        <o base=".bool" name="b">
      <o base=".eolang">                               <o base=".eolang">
        <o base=".org">                                  <o base="org">
          <o base="Q"></o>                                 <o base="Q"/>
        </o>                                             </o>
      </o>                                             </o>
      <o base="org.eolang.bool" data="bytes">    =>    <o base="org.eolang.bool">
        01                                               <o base="org.eolang.bytes" data="bytes">
      </o>                                                 01
    </o>                                                 </o>
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
  <xsl:template match="//o[@data and not(@base='org.eolang.bytes' or @base='bytes')]">
    <xsl:choose>
      <xsl:when test="parent::*[$literal-objects/text()=@base or ($reversed/text()=@base and o[@base='.eolang' and o[@base='.org' and o[@base='Q']]])]">
        <o base="org.eolang.bytes">
          <xsl:attribute name="data">
            <xsl:value-of select="./@data"/>
          </xsl:attribute>
          <xsl:value-of select="."/>
        </o>
      </xsl:when>
      <xsl:when test="parent::*[not(@base) or ($literal-objects/text()!=@base and $reversed/text()!=@base)]">
        <o>
          <xsl:for-each select="@*[name()!='data']">
            <xsl:attribute name="{name()}">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:for-each>
          <o base="org.eolang.bytes">
            <xsl:attribute name="data">
              <xsl:value-of select="./@data"/>
            </xsl:attribute>
            <xsl:value-of select="."/>
          </o>
        </o>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="//o[((@base='.bytes' and o[@base='.eolang' and o[@base='.org' and o[@base='Q']]]) or @base='org.eolang.bytes') and o[last() and @data]]">
    <o base="org.eolang.bytes">
      <xsl:for-each select="@*[name()!='data' and name()!='base']">
        <xsl:attribute name="{name()}">
          <xsl:value-of select="."/>
        </xsl:attribute>
      </xsl:for-each>
      <xsl:attribute name="data">
        <xsl:text>bytes</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="o[@data]/text()"/>
    </o>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
