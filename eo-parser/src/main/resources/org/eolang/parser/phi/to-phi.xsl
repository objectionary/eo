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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="https://www.eolang.org" id="to-phi" version="2.0">
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="text"/>
  <xsl:param name="conservative" as="xs:boolean"/>
  <!-- Variables -->
  <xsl:variable name="aliases" select="program/metas/meta/part[last()]"/>
  <xsl:variable name="number-pattern" select="'^[0-9]+$'"/>
  <xsl:variable name="xi">
    <select>ξ</select>
  </xsl:variable>
  <xsl:variable name="delta">
    <select>Δ</select>
  </xsl:variable>
  <xsl:variable name="alpha">
    <select>α</select>
  </xsl:variable>
  <xsl:variable name="phi">
    <select>φ</select>
  </xsl:variable>
  <xsl:variable name="rho">
    <select>ρ</select>
  </xsl:variable>
  <xsl:variable name="program">
    <select>Φ</select>
  </xsl:variable>
  <xsl:variable name="def-package">
    <select>Φ̇</select>
  </xsl:variable>
  <xsl:variable name="lambda">
    <select>λ</select>
  </xsl:variable>
  <xsl:variable name="arrow">
    <xsl:value-of select="$space"/>
    <select>↦</select>
    <xsl:value-of select="$space"/>
  </xsl:variable>
  <xsl:variable name="dashed-arrow">
    <xsl:value-of select="$space"/>
    <select>⤍</select>
    <xsl:value-of select="$space"/>
  </xsl:variable>
  <xsl:variable name="lb">
    <select>⟦</select>
  </xsl:variable>
  <xsl:variable name="rb">
    <select>⟧</select>
  </xsl:variable>
  <xsl:variable name="clb">
    <select>(</select>
  </xsl:variable>
  <xsl:variable name="crb">
    <select>)</select>
  </xsl:variable>
  <xsl:variable name="empty">
    <select>∅</select>
  </xsl:variable>
  <xsl:variable name="space">
    <xsl:text> </xsl:text>
  </xsl:variable>
  <xsl:variable name="new-line">
    <xsl:value-of select="'&#10;'"/>
  </xsl:variable>
  <!-- Functions -->
  <!-- ADD XI OR NOT -->
  <xsl:function name="eo:add-xi">
    <xsl:param name="add"/>
    <xsl:if test="$add">
      <xsl:value-of select="$xi"/>
      <xsl:text>.</xsl:text>
    </xsl:if>
  </xsl:function>
  <!-- Get clean escaped object name  -->
  <xsl:function name="eo:lambda-name">
    <xsl:param name="name"/>
    <xsl:value-of select="concat('L', replace(replace(replace(substring($name, 3), '\.', '_'), '-', '_'), '@', 'φ'))"/>
  </xsl:function>
  <!-- SPECIAL CHARACTERS -->
  <xsl:function name="eo:specials">
    <xsl:param name="n"/>
    <xsl:param name="is-name"/>
    <xsl:choose>
      <xsl:when test="$n='@'">
        <xsl:value-of select="eo:add-xi(not($is-name))"/>
        <xsl:value-of select="$phi"/>
      </xsl:when>
      <xsl:when test="$n='.@'">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$phi"/>
      </xsl:when>
      <xsl:when test="$n='^'">
        <xsl:value-of select="eo:add-xi(not($is-name))"/>
        <xsl:value-of select="$rho"/>
      </xsl:when>
      <xsl:when test="$n='.^'">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$rho"/>
      </xsl:when>
      <xsl:when test="$n='$'">
        <xsl:value-of select="$xi"/>
      </xsl:when>
      <xsl:when test="$n='Q'">
        <xsl:value-of select="$program"/>
      </xsl:when>
      <xsl:when test="starts-with($n, 'org.eolang')">
        <xsl:value-of select="$def-package"/>
        <xsl:value-of select="substring-after($n, 'org.eolang')"/>
      </xsl:when>
      <xsl:when test="$aliases[text()=$n]">
        <xsl:value-of select="$program"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$n"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="eo:add-xi(not($is-name))"/>
        <xsl:value-of select="$n"/>
      </xsl:otherwise>
    </xsl:choose>
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
    <xsl:value-of select="$new-line"/>
    <xsl:for-each select="1 to $tabs">
      <xsl:text>  </xsl:text>
    </xsl:for-each>
  </xsl:function>
  <!-- CAN WE PRINT GIVEN STRING IN SINGLE LINE -->
  <xsl:function name="eo:can-inline">
    <xsl:param name="str"/>
    <xsl:param name="extra"/>
    <xsl:sequence select="not(contains($str, $new-line)) and string-length($str) + $extra &lt; 80"/>
  </xsl:function>
  <!-- PRINT ARGUMENTS OF APPLICATION -->
  <xsl:function name="eo:print-args">
    <xsl:param name="current"/>
    <xsl:param name="accum"/>
    <xsl:param name="tabs"/>
    <xsl:param name="inlined"/>
    <xsl:choose>
      <xsl:when test="exists($current)">
        <xsl:variable name="applied">
          <xsl:apply-templates select="$current">
            <xsl:with-param name="tabs" select="$tabs"/>
            <xsl:with-param name="no-binding" select="true()"/>
          </xsl:apply-templates>
        </xsl:variable>
        <xsl:variable name="joined">
          <xsl:if test="string-length($accum)&gt;0">
            <xsl:value-of select="$accum"/>
            <xsl:text>||</xsl:text>
          </xsl:if>
          <xsl:value-of select="$applied"/>
        </xsl:variable>
        <xsl:choose>
          <xsl:when test="$inlined=true() and eo:can-inline($joined, 0)">
            <xsl:value-of select="eo:print-args($current/following-sibling::o[1], $joined, $tabs, true())"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="eo:print-args($current/following-sibling::o[1], $joined, $tabs, false())"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="contains($accum, '||')">
            <xsl:for-each select="tokenize($accum, '\|\|')">
              <xsl:if test="position()&gt;1">
                <xsl:value-of select="eo:comma(position(), -1)"/>
                <xsl:choose>
                  <xsl:when test="$inlined">
                    <xsl:value-of select="$space"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:value-of select="eo:eol($tabs)"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:if>
              <xsl:value-of select="."/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$accum"/>
          </xsl:otherwise>
        </xsl:choose>
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
          <xsl:when test="starts-with($object/@as, $alpha)">
            <xsl:value-of select="number(substring-after($object/@as, $alpha))"/>
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
  <xsl:function name="eo:is-ordered">
    <xsl:param name="current"/>
    <xsl:param name="position"/>
    <xsl:variable name="current-pos" select="eo:element-binding($current, $position - 1)"/>
    <xsl:variable name="next" select="$current/following-sibling::o[1]"/>
    <xsl:variable name="next-pos" select="eo:element-binding($next, $position)"/>
    <xsl:choose>
      <xsl:when test="matches($current-pos, $number-pattern)">
        <xsl:choose>
          <xsl:when test="not($next)">
            <xsl:sequence select="true()"/>
          </xsl:when>
          <xsl:when test="matches($next-pos, $number-pattern) and $current-pos + 1 = $next-pos">
            <xsl:value-of select="eo:is-ordered($next, $position + 1)"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:sequence select="false()"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="false()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- Program -->
  <xsl:template match="program">
    <program>
      <xsl:copy-of select="./sheets"/>
      <xsl:copy-of select="./errors"/>
      <phi>
        <xsl:text>{</xsl:text>
        <xsl:value-of select="$lb"/>
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
              <xsl:if test="$conservative">
                <xsl:value-of select="$clb"/>
                <xsl:value-of select="$crb"/>
              </xsl:if>
              <xsl:value-of select="$arrow"/>
              <xsl:value-of select="$lb"/>
              <xsl:value-of select="eo:eol($tabs+position())"/>
            </xsl:for-each>
            <xsl:apply-templates select="objects">
              <xsl:with-param name="tabs" select="$tabs + $length + 1"/>
              <xsl:with-param name="package">
                <xsl:value-of select="$program"/>
                <xsl:text>.</xsl:text>
                <xsl:value-of select="$package"/>
              </xsl:with-param>
            </xsl:apply-templates>
            <xsl:for-each select="$parts">
              <xsl:value-of select="eo:comma(2, $tabs + $length + 2 - position())"/>
              <xsl:value-of select="$lambda"/>
              <xsl:value-of select="$dashed-arrow"/>
              <xsl:text>Package</xsl:text>
              <xsl:value-of select="eo:eol($tabs + $length + 1 - position())"/>
              <xsl:value-of select="$rb"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="objects">
              <xsl:with-param name="tabs" select="$tabs"/>
              <xsl:with-param name="package" select="$program"/>
            </xsl:apply-templates>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:value-of select="eo:eol(0)"/>
        <xsl:value-of select="$rb"/>
        <xsl:text>}</xsl:text>
      </phi>
    </program>
  </xsl:template>
  <!-- Objects  -->
  <xsl:template match="objects">
    <xsl:param name="tabs"/>
    <xsl:param name="package"/>
    <xsl:for-each select="o">
      <xsl:value-of select="eo:comma(position(), $tabs)"/>
      <xsl:apply-templates select=".">
        <xsl:with-param name="tabs" select="$tabs"/>
        <xsl:with-param name="package" select="$package"/>
        <xsl:with-param name="before" select="$tabs * 2"/>
      </xsl:apply-templates>
    </xsl:for-each>
  </xsl:template>
  <!-- Void attribute -->
  <xsl:template match="o[eo:void(.)]">
    <xsl:value-of select="eo:specials(@name, true())"/>
    <xsl:value-of select="$arrow"/>
    <xsl:value-of select="$empty"/>
  </xsl:template>
  <!-- Inline void-attributes -->
  <xsl:template match="o" mode="inline-voids">
    <xsl:value-of select="$clb"/>
    <xsl:for-each select="o[eo:void(.)]">
      <xsl:if test="position()&gt;1">
        <xsl:value-of select="eo:comma(position(), -1)"/>
        <xsl:value-of select="$space"/>
      </xsl:if>
      <xsl:value-of select="eo:specials(@name, true())"/>
    </xsl:for-each>
    <xsl:value-of select="$crb"/>
  </xsl:template>
  <!-- Find path template -->
  <xsl:template match="*" mode="path">
    <xsl:param name="find"/>
    <xsl:variable name="parent" select="parent::*"/>
    <xsl:variable name="rho-dot">
      <xsl:value-of select="eo:specials('^', true())"/>
      <xsl:text>.</xsl:text>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="eo:abstract($parent)">
        <xsl:if test="not($parent/o[@name=$find])">
          <xsl:value-of select="$rho-dot"/>
          <xsl:apply-templates select="$parent" mode="path">
            <xsl:with-param name="find" select="$find"/>
          </xsl:apply-templates>
        </xsl:if>
      </xsl:when>
      <xsl:when test="not($parent[name()='objects'])">
        <xsl:apply-templates select="$parent" mode="path">
          <xsl:with-param name="find" select="$find"/>
        </xsl:apply-templates>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <!-- Just object -->
  <xsl:template match="o[@base and not(eo:void(.))]">
    <xsl:param name="tabs"/>
    <xsl:param name="package"/>
    <xsl:param name="before" select="$tabs * 2"/>
    <xsl:variable name="name">
      <xsl:if test="@name">
        <xsl:value-of select="eo:specials(@name, true())"/>
        <xsl:value-of select="$arrow"/>
      </xsl:if>
    </xsl:variable>
    <xsl:value-of select="$name"/>
    <xsl:variable name="after-name" select="$before + string-length($name)"/>
    <xsl:choose>
      <!-- Not method -->
      <xsl:when test="not(starts-with(@base, '.'))">
        <xsl:variable name="start">
          <xsl:choose>
            <xsl:when test="@ref and not(eo:has-data(.))">
              <xsl:value-of select="eo:add-xi(true())"/>
              <xsl:apply-templates select="." mode="path">
                <xsl:with-param name="find" select="@base"/>
              </xsl:apply-templates>
              <xsl:value-of select="eo:specials(@base, true())"/>
            </xsl:when>
            <xsl:when test="eo:has-data(.) and (@base='org.eolang.number' or @base='org.eolang.string')">
              <xsl:value-of select="text()"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="eo:specials(@base, false())"/>
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
        <xsl:variable name="method" select="eo:specials(@base, true())"/>
        <xsl:value-of select="$start"/>
        <xsl:value-of select="$method"/>
        <xsl:variable name="after-start">
          <xsl:choose>
            <xsl:when test="contains(string-join(($start, $method), ''), $new-line)">
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
    <xsl:if test="eo:has-data(.) and @base!='org.eolang.number' and @base!='org.eolang.string'">
      <xsl:value-of select="$clb"/>
      <xsl:value-of select="$lb"/>
      <xsl:value-of select="$space"/>
      <xsl:value-of select="$delta"/>
      <xsl:value-of select="$dashed-arrow"/>
      <xsl:value-of select="text()[last()]"/>
      <xsl:value-of select="$space"/>
      <xsl:value-of select="$rb"/>
      <xsl:value-of select="$crb"/>
    </xsl:if>
  </xsl:template>
  <!-- Nestd applied objects -->
  <xsl:template match="o" mode="nested">
    <xsl:param name="tabs"/>
    <xsl:param name="after-start"/>
    <xsl:param name="o-position"/>
    <xsl:param name="nested"/>
    <xsl:value-of select="$clb"/>
    <xsl:variable name="applied">
      <xsl:choose>
        <xsl:when test="eo:is-ordered(o[position()=$o-position][1], $o-position)">
          <xsl:value-of select="eo:print-args(o[position()=$o-position][1], '', $tabs + 1, true())"/>
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
    <xsl:value-of select="$crb"/>
  </xsl:template>
  <!-- Formation -->
  <xsl:template match="o[eo:abstract(.)]">
    <xsl:param name="tabs"/>
    <xsl:param name="package"/>
    <xsl:param name="no-binding" select="false()"/>
    <xsl:variable name="name" select="eo:specials(@name, true())"/>
    <xsl:if test="@name">
      <xsl:value-of select="$name"/>
      <xsl:if test="$conservative or count(o[eo:void(.)])&gt;0">
        <xsl:apply-templates select="." mode="inline-voids"/>
      </xsl:if>
      <xsl:value-of select="$arrow"/>
    </xsl:if>
    <xsl:value-of select="$lb"/>
    <!-- Atom or not empty formation -->
    <xsl:variable name="inners" select="if ($no-binding or (not(@as) and not(@name))) then o else o[not(eo:void(.))]"/>
    <xsl:if test="@atom or count($inners)&gt;0">
      <xsl:value-of select="eo:eol($tabs+1)"/>
      <!-- Atom -->
      <xsl:if test="@atom">
        <xsl:variable name="lambda-name">
          <xsl:value-of select="$package"/>
          <xsl:text>.</xsl:text>
          <xsl:value-of select="$name"/>
        </xsl:variable>
        <xsl:value-of select="$lambda"/>
        <xsl:value-of select="$dashed-arrow"/>
        <xsl:value-of select="eo:lambda-name($lambda-name)"/>
        <xsl:if test="count($inners)&gt;0">
          <xsl:value-of select="eo:comma(2, $tabs+1)"/>
        </xsl:if>
      </xsl:if>
      <!-- Inner objects -->
      <xsl:for-each select="$inners">
        <xsl:value-of select="eo:comma(position(), $tabs+1)"/>
        <xsl:apply-templates select=".">
          <xsl:with-param name="tabs" select="$tabs+1"/>
          <xsl:with-param name="package">
            <xsl:value-of select="$package"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="$name"/>
          </xsl:with-param>
          <xsl:with-param name="before" select="$tabs * 2"/>
        </xsl:apply-templates>
      </xsl:for-each>
      <xsl:value-of select="eo:eol($tabs)"/>
    </xsl:if>
    <xsl:value-of select="$rb"/>
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
          <xsl:value-of select="eo:specials(@as, true())"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$alpha"/>
          <xsl:value-of select="$position - 1"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="eo:abstract(.) and ($conservative or o[eo:void(.)])">
        <xsl:apply-templates select="." mode="inline-voids"/>
      </xsl:if>
      <xsl:value-of select="$arrow"/>
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
