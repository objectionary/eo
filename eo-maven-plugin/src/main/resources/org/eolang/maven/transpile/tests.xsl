<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="tests" version="2.0">
  <!--
  This stylesheet will take outer objects and put them into
  classes that are unit tests.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:function name="eo:test-name" as="xs:string">
    <xsl:param name="name" as="xs:string"/>
    <xsl:variable name="parts" select="tokenize($name, '\$')"/>
    <xsl:variable name="p">
      <xsl:for-each select="$parts">
        <xsl:if test="position()&gt;1">
          <xsl:text>$</xsl:text>
        </xsl:if>
        <xsl:value-of select="."/>
        <xsl:if test="position()=1">
          <xsl:text>Test</xsl:text>
        </xsl:if>
      </xsl:for-each>
    </xsl:variable>
    <xsl:value-of select="$p"/>
  </xsl:function>
  <xsl:template match="class/@name">
    <xsl:attribute name="name">
      <xsl:choose>
        <xsl:when test="/object/metas/meta[head='tests']">
          <xsl:value-of select="eo:test-name(.)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
