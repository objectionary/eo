<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="explicit-data" version="2.0">
  <!--
  Here we transform just data into application
  - 5             => number 5
  - number 5      => number 5
  - 42.2          => number 42.2
  - number 22.4   => number 22.4
  - "Hey"         => string "Hey"
  - QQ.string "H" => QQ.string "H"

  In the level of xmir it looks like:
  - <o base="Q.org.eolang.bytes">                      <o base="Q.org.eolang.bytes">
      22-32                                      =>    22-32
    </o>                                             </o>
  - <o base="Q.org.eolang.number" name="num">          <o base="Q.org.eolang.number" name="num">
      <o base="Q.org.eolang.number">                     <o base=Q.org.eolang.bytes>
        00 00 00 00 00 00 00 01                  =>      00 00 00 00 00 00 00 01
      </o>                                             </o>
    </o>                                             </o>
  - <o base=".number" name="b">                      <o base=".number" name="b">
      <o base=".eolang">                               <o base=".eolang">
        <o base=".org">                                  <o base="org">
          <o base="Q"></o>                                 <o base="Q"/>
        </o>                                             </o>
      </o>                                             </o>
      <o base="Q.org.eolang.number">               =>    <o base="Q.org.eolang.bytes">
        01                                               01
      </o>                                             </o>
    </o>                                             </o>
  -->
  <xsl:import href="/org/eolang/parser/_datas.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="reversed" as="element()*">
    <xsl:for-each select="$literal-objects">
      <a>
        <xsl:value-of select="replace(./text(), 'Q\.org\.eolang\.', '.')"/>
      </a>
    </xsl:for-each>
  </xsl:variable>
  <xsl:template match="o[not(o)]">
    <xsl:apply-templates select="." mode="no-children"/>
  </xsl:template>
  <xsl:template match="o[string-length(normalize-space(text())) &gt; 0]" mode="no-children">
    <xsl:apply-templates select="." mode="with-data"/>
  </xsl:template>
  <xsl:template match="o[not(@base='Q.org.eolang.bytes')]" mode="with-data">
    <xsl:choose>
      <xsl:when test="parent::*[$literal-objects/text()=@base or ($reversed/text()=@base and o[1][@base='.eolang' and o[1][@base='.org' and o[1][@base='Q']]])]">
        <o base="Q.org.eolang.bytes">
          <xsl:value-of select="."/>
        </o>
      </xsl:when>
      <xsl:when test="parent::*[not(@base) or ($literal-objects/text()!=@base and $reversed/text()!=@base)]">
        <o>
          <xsl:for-each select="@*">
            <xsl:attribute name="{name()}">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:for-each>
          <o base="Q.org.eolang.bytes">
            <xsl:value-of select="."/>
          </o>
        </o>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="o[@base='Q.org.eolang.bytes']" mode="with-data">
    <o base="Q.org.eolang.bytes">
      <xsl:for-each select="@*[name()!='base']">
        <xsl:attribute name="{name()}">
          <xsl:value-of select="."/>
        </xsl:attribute>
      </xsl:for-each>
      <xsl:value-of select="text()"/>
    </o>
  </xsl:template>
  <xsl:template match="o[((@base='.bytes' and o[1][@base='.eolang' and o[1][@base='.org' and o[1][@base='Q']]]) or @base='org.eolang.bytes') and o[last() and not(o) and string-length(normalize-space(text())) &gt; 0]]">
    <o base="Q.org.eolang.bytes">
      <xsl:for-each select="@*[name()!='base']">
        <xsl:attribute name="{name()}">
          <xsl:value-of select="."/>
        </xsl:attribute>
      </xsl:for-each>
      <xsl:value-of select="o[last()]/text()"/>
    </o>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
