<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="add-root" version="2.0">
  <!--
  Here we insert ADD instruction to create the root "ν0" object.
  -->
  <xsl:import href="/org/eolang/maven/sodg/_macros.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object/sodg">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:call-template name="i">
        <xsl:with-param name="name" select="'ADD'"/>
        <xsl:with-param name="args" as="item()*">
          <xsl:sequence>
            <xsl:text>ν0</xsl:text>
          </xsl:sequence>
        </xsl:with-param>
        <xsl:with-param name="comment">
          <xsl:text>This is the root</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o" mode="sodg">
    <!-- ignore them -->
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*" mode="#current"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
