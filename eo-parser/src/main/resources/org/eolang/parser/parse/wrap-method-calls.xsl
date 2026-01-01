<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="wrap-method-calls" version="2.0">
  <!--
  When we see this structure:

  <o base="foo"/>
  <o base=".bar" method=""/>
  <o base=".test" method=""/>

  We transfer it to this one:

  <o base=".test"/>
    <o base=".bar"/>
      <o base="foo"/>
    </o>
  </o>
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@method]" mode="#all" priority="0">
    <xsl:variable name="target" select="preceding-sibling::o[1]"/>
    <xsl:copy>
      <xsl:apply-templates select="@* except @method"/>
      <xsl:apply-templates select="$target" mode="full"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[following-sibling::o[1][@method]]" priority="1">
    <!-- We delete the original one -->
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@* except @method"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
