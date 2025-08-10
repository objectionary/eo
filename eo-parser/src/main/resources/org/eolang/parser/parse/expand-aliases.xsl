<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="expand-aliases" version="2.0">
  <!--
  Here we find all aliases that don't use full syntax
  and expand them to the full one. For example, this one:

  +alias org.example.foo

  Will be expanded to:

  +alias foo org.example.foo
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object/metas/meta[head='alias']">
    <xsl:variable name="composite" select="contains(tail, ' ')"/>
    <xsl:variable name="split" select="tokenize(tail/text(), ' ')"/>
    <xsl:variable name="last">
      <xsl:choose>
        <xsl:when test="$composite">
          <xsl:value-of select="$split[last()]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="tail/text()"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="first">
      <xsl:choose>
        <xsl:when test="$composite">
          <xsl:value-of select="$split[1]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:choose>
            <xsl:when test="contains($last, '.')">
              <xsl:value-of select="tokenize($last, '\.')[last()]"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="tail/text()"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="tail">
      <xsl:if test="not(starts-with($last, 'Φ.')) and not(starts-with($last, 'Φ̇.'))">
        <xsl:text>Φ.</xsl:text>
      </xsl:if>
      <xsl:value-of select="$last"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="node() except tail except part|@*"/>
      <xsl:element name="tail">
        <xsl:value-of select="$first"/>
        <xsl:if test="$composite">
          <xsl:for-each select="$split[position()&gt;1 and position()!=last()]">
            <xsl:text> </xsl:text>
            <xsl:value-of select="."/>
          </xsl:for-each>
        </xsl:if>
        <xsl:text> </xsl:text>
        <xsl:value-of select="$tail"/>
      </xsl:element>
      <xsl:element name="part">
        <xsl:value-of select="$first"/>
      </xsl:element>
      <xsl:if test="$composite">
        <xsl:for-each select="$split[position()&gt;1 and position()!=last()]">
          <xsl:element name="part">
            <xsl:value-of select="."/>
          </xsl:element>
        </xsl:for-each>
      </xsl:if>
      <xsl:element name="part">
        <xsl:value-of select="$tail"/>
      </xsl:element>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
