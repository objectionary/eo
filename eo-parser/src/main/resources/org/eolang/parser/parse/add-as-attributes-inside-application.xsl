<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="add-as-attributes-inside-application" version="2.0">
  <!--
  Here we are going through all the applications (objects with `@base`), and add `@as`
  attributes for each inner object. The content of `@as` attribute is based on `αN`, where `N` is
  an identifier of attribute in the current level of nesting. Object methods (objects with `@base`
  starting with `.` (dot) must not have `@as` attributes.
  -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[@base and not(starts-with(@base, '.'))]/o[not(@as)]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:attribute name="as">
        <xsl:text>α</xsl:text>
        <xsl:value-of select="count(preceding-sibling::o[not(@as)])"/>
      </xsl:attribute>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
