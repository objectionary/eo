<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="add-sodg-root" version="2.0">
  <!--
  Here we start the graph, creating a new XML element "sodg" under "program".
  All further XSL transformations will work with "i" elements inside
  this "sodg" one.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/program[not(sodg)]">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:element name="sodg">
        <!-- empty one -->
      </xsl:element>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#default">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
