<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="clean-up" version="2.0">
  <!--
  Here, we remove all the temporary information XMIR
  that doesn't match the requirements of XSD schema.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="@ancestors | @original-name | @parent">
    <!-- nothing -->
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
