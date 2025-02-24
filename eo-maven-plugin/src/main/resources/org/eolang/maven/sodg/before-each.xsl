<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="before-each" version="2.0">
  <!--
  Here we add a logging line to the list of instructions before
  each XSL stylesheet.
  -->
  <xsl:import href="/org/eolang/maven/sodg/_macros.xsl"/>
  <xsl:param name="sheet"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program/sodg">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:call-template name="i">
        <xsl:with-param name="name" select="'COMMENT'"/>
        <xsl:with-param name="args" as="item()*">
          <!-- nothing here -->
        </xsl:with-param>
        <xsl:with-param name="comment">
          <xsl:value-of select="$sheet"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
