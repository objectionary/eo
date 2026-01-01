<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs" id="validate-objects-count" version="2.0">
  <!--
    Here we add error with severity 'critical' is there's not exactly 1 object
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object">
    <xsl:variable name="error" as="element()*">
      <xsl:if test="count(o)&gt;1">
        <xsl:element name="error">
          <xsl:attribute name="check" select="'validate-objects-count'"/>
          <xsl:attribute name="line" select="if (o[2]/@line) then o[2]/@line else 0"/>
          <xsl:attribute name="severity" select="'critical'"/>
          <xsl:text>Only one object per source file is allowed and required</xsl:text>
        </xsl:element>
      </xsl:if>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="(node() except errors)|@*"/>
      <xsl:if test="exists($error) or exists(/object/errors)">
        <errors>
          <xsl:apply-templates select="/object/errors/error"/>
          <xsl:copy-of select="$error"/>
        </errors>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
