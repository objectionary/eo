<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="remove-this" version="2.0">
  <!--
    Here we change elements:

    <o base="$.a.b.c"/>

    with:

    <o base="a.b.c"/>
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[starts-with(@base, '$.')]">
    <xsl:copy>
      <xsl:attribute name="base" select="substring-after(@base, '$.')"/>
      <xsl:apply-templates select="node()|(@* except @base)"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
