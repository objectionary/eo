<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="unnecessary-as" version="2.0">
  <!--
  Performs the reverse operation of "/org/eolang/parser/mandatory-as.xsl"
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:function name="eo:all-alphas">
    <xsl:param name="index"/>
    <xsl:param name="elem"/>
    <xsl:choose>
      <xsl:when test="$elem/@as=concat('α', string($index))">
        <xsl:choose>
          <xsl:when test="$elem/following-sibling::o">
            <xsl:sequence select="eo:all-alphas($index + 1, $elem/following-sibling::o)"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:sequence select="true()"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="false()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:template match="o[@base and not(starts-with(@base, '.')) and count(o[@as])=count(o)]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:choose>
        <xsl:when test="eo:all-alphas(0, o[1])">
          <xsl:for-each select="o">
            <xsl:variable name="elem">
              <xsl:element name="o">
                <xsl:for-each select="@*[name()!='as']">
                  <xsl:attribute name="{name()}" select="."/>
                </xsl:for-each>
                <xsl:apply-templates select="node()"/>
              </xsl:element>
            </xsl:variable>
            <xsl:apply-templates select="$elem"/>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="o"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:apply-templates select="text()"/>
    </xsl:copy>
  </xsl:template>
  <!--
  A reversed dispatch (`.method`, base starting with a dot) whose leading
  children are the receiver and whose trailing children are the positional
  arguments carrying `@as`. Normally the receiver is a single child, but a
  `>>` handle applied as this receiver relocates into two leading children —
  the formation predecessor and its `| args` pipe continuation (#5844) — so
  the count of leading receiver children is whatever is left once the `@as`
  arguments are removed. Emit those leading children untouched, then, if the
  arguments form the canonical `α0, α1, …` run, strip their now-redundant
  `@as`.
  -->
  <xsl:template match="o[@base and starts-with(@base, '.') and exists(o[@as]) and (count(o[@as])=count(o)-1 or (count(o[@as])=count(o)-2 and exists(o[@pipe])))]">
    <xsl:variable name="lead" select="count(o) - count(o[@as])"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="o[position()&lt;=$lead]"/>
      <xsl:choose>
        <xsl:when test="eo:all-alphas(0, o[position()=$lead+1])">
          <xsl:for-each select="o[position()&gt;$lead]">
            <xsl:variable name="elem">
              <xsl:element name="o">
                <xsl:for-each select="@*[name()!='as']">
                  <xsl:attribute name="{name()}" select="."/>
                </xsl:for-each>
                <xsl:apply-templates select="node()"/>
              </xsl:element>
            </xsl:variable>
            <xsl:apply-templates select="$elem"/>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="o[position()&gt;$lead]"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:apply-templates select="text()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
