<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="package" version="2.0">
  <!--
  This stylesheet will set the package name for each
  <class> element, using the information from the +package meta.
  -->
  <xsl:template match="class[not(@package)]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:attribute name="package">
        <xsl:value-of select="/object/metas/meta[head='package'][1]/tail"/>
      </xsl:attribute>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
