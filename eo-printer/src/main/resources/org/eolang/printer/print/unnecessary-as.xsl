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
  <xsl:template match="o[@base and starts-with(@base, '.') and count(o[@as])=count(o)-1]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="o[1]"/>
      <xsl:choose>
        <xsl:when test="eo:all-alphas(0, o[position()=2])">
          <xsl:for-each select="o[position()&gt;1]">
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
          <xsl:apply-templates select="o[position()&gt;1]"/>
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
