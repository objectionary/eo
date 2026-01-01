<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" id="blank-xsd-schema" version="2.0">
  <!--
  This stylesheet removes reference to the XSD schema from the XML document.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object/@xsi:noNamespaceSchemaLocation">
    <xsl:attribute name="xsi:noNamespaceSchemaLocation">
      <xsl:text>https://www.eolang.org/xsd/XMIR-anything.xsd</xsl:text>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
