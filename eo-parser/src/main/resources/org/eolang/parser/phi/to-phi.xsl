<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="https://www.eolang.org" id="to-phi" version="2.0">
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="text"/>
  <!-- Variables -->
  <xsl:variable name="aliases" select="object/metas/meta/part[last()]"/>
  <xsl:variable name="number-pattern" select="'^[0-9]+$'"/>
  <!-- Functions -->
  <!-- Get clean escaped object name  -->
  <xsl:function name="eo:lambda-name">
    <xsl:param name="name"/>
    <xsl:value-of select="concat('L', replace(replace(replace(substring($name, 3), '\.', '_'), '-', '_'), '@', $eo:phi))"/>
  </xsl:function>
  <!-- SPECIAL CHARACTERS -->
  <xsl:function name="eo:specials">
    <xsl:param name="n"/>
    <xsl:variable name="result">
      <xsl:choose>
        <xsl:when test="$n='@'">
          <xsl:value-of select="$eo:phi"/>
        </xsl:when>
        <xsl:when test="$n='.@'">
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$eo:phi"/>
        </xsl:when>
        <xsl:when test="$n='^'">
          <xsl:value-of select="$eo:rho"/>
        </xsl:when>
        <xsl:when test="$n='.^'">
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$eo:rho"/>
        </xsl:when>
        <xsl:when test="$n='$'">
          <xsl:value-of select="$eo:xi"/>
        </xsl:when>
        <xsl:when test="$n='Q'">
          <xsl:value-of select="$eo:program"/>
        </xsl:when>
        <xsl:when test="starts-with($n, 'Q.org.eolang')">
          <xsl:value-of select="$eo:def-package"/>
          <xsl:value-of select="substring-after($n, 'Q.org.eolang')"/>
        </xsl:when>
        <xsl:when test="$aliases[text()=$n] and not(starts-with($n, 'j$'))">
          <xsl:value-of select="$eo:program"/>
          <xsl:text>.</xsl:text>
          <xsl:choose>
            <xsl:when test="starts-with($n, 'Q.')">
              <xsl:value-of select="substring($n, 3)"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$n"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:when test="contains($n, '.')">
          <xsl:for-each select="tokenize($n, '\.')">
            <xsl:if test="position()&gt;1">
              <xsl:text>.</xsl:text>
            </xsl:if>
            <xsl:value-of select="eo:specials(.)"/>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$n"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="string($result)"/>
  </xsl:function>
  <!-- COMMA WITH SPACE -->
  <xsl:function name="eo:comma">
    <xsl:param name="pos"/>
    <xsl:param name="tabs"/>
    <xsl:if test="$pos&gt;1">
      <xsl:text>,</xsl:text>
      <xsl:if test="$tabs != -1">
        <xsl:value-of select="eo:eol($tabs)"/>
      </xsl:if>
    </xsl:if>
  </xsl:function>
  <!-- EOL WITH INDENTATION -->
  <xsl:function name="eo:eol">
    <xsl:param name="tabs"/>
    <xsl:value-of select="$eo:new-line"/>
    <xsl:for-each select="1 to $tabs">
      <xsl:text>  </xsl:text>
    </xsl:for-each>
  </xsl:function>
  <!-- CAN WE PRINT GIVEN STRING IN SINGLE LINE -->
  <xsl:function name="eo:can-inline" as="xs:boolean">
    <xsl:param name="str"/>
    <xsl:param name="extra"/>
    <xsl:sequence select="not(contains($str, $eo:new-line)) and string-length($str) + $extra &lt; 80"/>
  </xsl:function>
  <!-- PRINT ARGUMENTS OF APPLICATION -->
  <xsl:function name="eo:print-args">
    <xsl:param name="objects"/>
    <xsl:param name="tabs"/>
    <xsl:variable name="elements" as="xs:string*">
      <xsl:for-each select="$objects">
        <xsl:variable name="applied">
          <xsl:apply-templates select=".">
            <xsl:with-param name="tabs" select="$tabs"/>
            <xsl:with-param name="no-binding" select="true()"/>
          </xsl:apply-templates>
        </xsl:variable>
        <xsl:value-of select="$applied"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="eo:can-inline(string-join($elements, ', '), 0)">
        <xsl:for-each select="$elements">
          <xsl:if test="position()&gt;1">
            <xsl:value-of select="eo:comma(position(), -1)"/>
            <xsl:value-of select="$eo:space"/>
          </xsl:if>
          <xsl:value-of select="."/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="$elements">
          <xsl:if test="position()&gt;1">
            <xsl:value-of select="eo:comma(position(), -1)"/>
            <xsl:value-of select="eo:eol($tabs)"/>
          </xsl:if>
          <xsl:value-of select="."/>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
    Try to get @as attribute from the object and convert to number, if @as is absent - return position
  -->
  <xsl:function name="eo:element-binding">
    <xsl:param name="object"/>
    <xsl:param name="position"/>
    <xsl:choose>
      <xsl:when test="$object/@as">
        <xsl:choose>
          <xsl:when test="starts-with($object/@as, $eo:alpha)">
            <xsl:value-of select="number(substring-after($object/@as, $eo:alpha))"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$object/@as"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$position"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
    CHECKS IF APPLICATION ARGUMENTS IS RIGHT ORDER WITH ALPHAS, e.g.
    α0 -> a, α1 -> b, ..., αN -> z
  -->
  <xsl:function name="eo:is-ordered" as="xs:boolean">
    <xsl:param name="objects" as="element()*"/>
    <xsl:variable name="generated">
      <xsl:for-each select="$objects">
        <xsl:value-of select="xs:string(eo:element-binding(., position() - 1))"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="template">
      <xsl:for-each select="0 to (count($objects) - 1)">
        <xsl:value-of select="xs:string(.)"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:value-of select="string-join($generated, '')=string-join($template, '')"/>
  </xsl:function>
  <!-- Program -->
  <xsl:template match="object">
    <xsl:copy>
      <xsl:apply-templates select="sheets|errors"/>
      <xsl:copy-of select="o[1]"/>
      <phi>
        <xsl:text>{</xsl:text>
        <xsl:value-of select="$eo:lb"/>
        <xsl:variable name="tabs" select="1"/>
        <xsl:value-of select="eo:eol(1)"/>
        <xsl:variable name="has-package" select="metas/meta/head[text()='package']"/>
        <xsl:variable name="package" select="metas/meta[head[text()='package']]/tail[1]"/>
        <xsl:variable name="parts" select="tokenize($package,'\.')"/>
        <xsl:variable name="length" select="string-length($package)-string-length(replace($package,'\.',''))"/>
        <xsl:choose>
          <xsl:when test="$has-package">
            <xsl:for-each select="$parts">
              <xsl:value-of select="."/>
              <xsl:value-of select="$eo:arrow"/>
              <xsl:value-of select="$eo:lb"/>
              <xsl:value-of select="eo:eol($tabs+position())"/>
            </xsl:for-each>
            <xsl:apply-templates select="o[1]">
              <xsl:with-param name="tabs" select="$tabs + $length + 1"/>
              <xsl:with-param name="package">
                <xsl:value-of select="$eo:program"/>
                <xsl:text>.</xsl:text>
                <xsl:value-of select="$package"/>
              </xsl:with-param>
              <xsl:with-param name="before" select="($tabs + $length + 1) * 2"/>
            </xsl:apply-templates>
            <xsl:for-each select="$parts">
              <xsl:value-of select="eo:comma(2, $tabs + $length + 2 - position())"/>
              <xsl:value-of select="$eo:lambda"/>
              <xsl:value-of select="$eo:dashed-arrow"/>
              <xsl:text>Package</xsl:text>
              <xsl:value-of select="eo:eol($tabs + $length + 1 - position())"/>
              <xsl:value-of select="$eo:rb"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="o[1]">
              <xsl:with-param name="tabs" select="$tabs"/>
              <xsl:with-param name="package" select="$eo:program"/>
              <xsl:with-param name="before" select="$tabs * 2"/>
            </xsl:apply-templates>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:value-of select="eo:eol(0)"/>
        <xsl:value-of select="$eo:rb"/>
        <xsl:text>}</xsl:text>
      </phi>
    </xsl:copy>
  </xsl:template>
  <!-- Void attribute -->
  <xsl:template match="o[eo:void(.)]">
    <xsl:value-of select="eo:specials(@name)"/>
    <xsl:value-of select="$eo:arrow"/>
    <xsl:value-of select="$eo:empty"/>
  </xsl:template>
  <!-- Inline void-attributes -->
  <xsl:template match="o" mode="inline-voids">
    <xsl:value-of select="$eo:clb"/>
    <xsl:for-each select="o[eo:void(.)]">
      <xsl:if test="position()&gt;1">
        <xsl:value-of select="eo:comma(position(), -1)"/>
        <xsl:value-of select="$eo:space"/>
      </xsl:if>
      <xsl:value-of select="eo:specials(@name)"/>
    </xsl:for-each>
    <xsl:value-of select="$eo:crb"/>
  </xsl:template>
  <!-- Just object -->
  <xsl:template match="o[@base and not(eo:void(.))]">
    <xsl:param name="tabs"/>
    <xsl:param name="package"/>
    <xsl:param name="before" select="$tabs * 2"/>
    <xsl:variable name="name">
      <xsl:if test="@name">
        <xsl:value-of select="eo:escape-plus(eo:specials(@name))"/>
        <xsl:value-of select="$eo:arrow"/>
      </xsl:if>
    </xsl:variable>
    <xsl:value-of select="$name"/>
    <xsl:variable name="after-name" select="$before + string-length($name)"/>
    <xsl:choose>
      <!-- Not method -->
      <xsl:when test="not(starts-with(@base, '.'))">
        <xsl:variable name="start">
          <xsl:choose>
            <xsl:when test="eo:has-data(.) and (@base='Q.org.eolang.number' or @base='Q.org.eolang.string')">
              <xsl:value-of select="eo:read-data(.)"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="eo:specials(@base)"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="$start"/>
        <xsl:variable name="after-start" select="$after-name + string-length($start)"/>
        <!-- Nested objects -->
        <xsl:if test="count(o)&gt;0">
          <xsl:apply-templates select="." mode="nested">
            <xsl:with-param name="tabs" select="$tabs"/>
            <xsl:with-param name="after-start" select="$after-start"/>
            <xsl:with-param name="o-position" select="1"/>
            <xsl:with-param name="nested" select="o"/>
          </xsl:apply-templates>
        </xsl:if>
      </xsl:when>
      <!-- Method -->
      <xsl:otherwise>
        <xsl:variable name="start">
          <xsl:apply-templates select="o[position()=1]">
            <xsl:with-param name="tabs" select="$tabs"/>
            <xsl:with-param name="package" select="$package"/>
            <xsl:with-param name="before" select="$after-name"/>
          </xsl:apply-templates>
        </xsl:variable>
        <xsl:variable name="method" select="eo:specials(@base)"/>
        <xsl:value-of select="$start"/>
        <xsl:value-of select="$method"/>
        <xsl:variable name="after-start">
          <xsl:choose>
            <xsl:when test="contains(string-join(($start, $method), ''), $eo:new-line)">
              <xsl:value-of select="$after-name + string-length($method[last()])"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$after-name + string-length($start) + string-length($method[last()])"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <!-- Nested objects -->
        <xsl:if test="count(o)&gt;1">
          <xsl:apply-templates select="." mode="nested">
            <xsl:with-param name="tabs" select="$tabs"/>
            <xsl:with-param name="after-start" select="$after-start"/>
            <xsl:with-param name="o-position" select="2"/>
            <xsl:with-param name="nested" select="o[position()!=1]"/>
          </xsl:apply-templates>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
    <!-- Data -->
    <xsl:if test="eo:has-data(.) and @base!='Q.org.eolang.number' and @base!='Q.org.eolang.string'">
      <xsl:value-of select="$eo:clb"/>
      <xsl:value-of select="$eo:lb"/>
      <xsl:value-of select="$eo:space"/>
      <xsl:value-of select="$eo:delta"/>
      <xsl:value-of select="$eo:dashed-arrow"/>
      <xsl:value-of select="text()[last()]"/>
      <xsl:value-of select="$eo:space"/>
      <xsl:value-of select="$eo:rb"/>
      <xsl:value-of select="$eo:crb"/>
    </xsl:if>
  </xsl:template>
  <!-- Nested applied objects -->
  <xsl:template match="o" mode="nested">
    <xsl:param name="tabs"/>
    <xsl:param name="after-start"/>
    <xsl:param name="nested"/>
    <xsl:variable name="applied">
      <xsl:choose>
        <xsl:when test="eo:is-ordered($nested)">
          <xsl:value-of select="eo:print-args($nested, $tabs + 1)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="$nested">
            <xsl:apply-templates select="." mode="application">
              <xsl:with-param name="position" select="position()"/>
              <xsl:with-param name="tabs" select="$tabs+1"/>
              <xsl:with-param name="before" select="($tabs + 1) * 2"/>
            </xsl:apply-templates>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$eo:clb"/>
    <xsl:choose>
      <xsl:when test="not(eo:can-inline($applied, $after-start))">
        <xsl:value-of select="eo:eol($tabs+1)"/>
        <xsl:value-of select="$applied"/>
        <xsl:value-of select="eo:eol($tabs)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$applied"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="$eo:crb"/>
  </xsl:template>
  <!-- Formation -->
  <xsl:template match="o[eo:abstract(.)]">
    <xsl:param name="tabs"/>
    <xsl:param name="package"/>
    <xsl:param name="no-binding" select="false()"/>
    <xsl:variable name="name" select="eo:specials(@name)"/>
    <xsl:if test="@name">
      <xsl:value-of select="eo:escape-plus($name)"/>
      <xsl:choose>
        <!-- Atom -->
        <xsl:when test="@name=$eo:lambda">
          <xsl:value-of select="$eo:dashed-arrow"/>
          <xsl:value-of select="eo:lambda-name($package)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:if test="count(o[eo:void(.)])&gt;0">
            <xsl:apply-templates select="." mode="inline-voids"/>
          </xsl:if>
          <xsl:value-of select="$eo:arrow"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:if test="not(@name) or @name!=$eo:lambda">
      <xsl:value-of select="$eo:lb"/>
      <!-- Atom or not empty formation -->
      <xsl:variable name="inners" select="if ($no-binding or (not(@as) and not(@name))) then o else o[not(eo:void(.))]"/>
      <xsl:if test="count($inners)&gt;0">
        <xsl:value-of select="eo:eol($tabs+1)"/>
        <!-- Inner objects -->
        <xsl:for-each select="$inners">
          <xsl:value-of select="eo:comma(position(), $tabs+1)"/>
          <xsl:apply-templates select=".">
            <xsl:with-param name="tabs" select="$tabs+1"/>
            <xsl:with-param name="package">
              <xsl:value-of select="$package"/>
              <xsl:text>.</xsl:text>
              <xsl:value-of select="eo:escape-plus($name)"/>
            </xsl:with-param>
            <xsl:with-param name="before" select="$tabs * 2"/>
          </xsl:apply-templates>
        </xsl:for-each>
        <xsl:value-of select="eo:eol($tabs)"/>
      </xsl:if>
      <xsl:value-of select="$eo:rb"/>
    </xsl:if>
  </xsl:template>
  <!-- Application -->
  <xsl:template match="o" mode="application">
    <xsl:param name="tabs"/>
    <xsl:param name="position" select="1"/>
    <xsl:param name="before"/>
    <xsl:variable name="applied">
      <xsl:value-of select="eo:comma($position, $tabs)"/>
      <xsl:choose>
        <xsl:when test="@as">
          <xsl:value-of select="eo:specials(@as)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$eo:alpha"/>
          <xsl:value-of select="$position - 1"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="eo:abstract(.) and o[eo:void(.)]">
        <xsl:apply-templates select="." mode="inline-voids"/>
      </xsl:if>
      <xsl:value-of select="$eo:arrow"/>
    </xsl:variable>
    <xsl:value-of select="$applied"/>
    <xsl:apply-templates select=".">
      <xsl:with-param name="tabs" select="$tabs"/>
      <xsl:with-param name="before" select="string-length($applied) + $before"/>
    </xsl:apply-templates>
  </xsl:template>
  <!-- Ignore other elements -->
  <xsl:template match="node()|@*">
    <xsl:apply-templates/>
  </xsl:template>
</xsl:stylesheet>
