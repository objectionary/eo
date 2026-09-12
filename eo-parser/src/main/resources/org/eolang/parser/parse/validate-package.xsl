<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="validate-package" version="2.0">
  <!-- A file has one package; flag every directive after the first. -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object">
    <xsl:variable name="errors" as="element()*">
      <xsl:for-each select="metas/meta[head='package'][position() &gt; 1]">
        <error check="validate-package" severity="critical">
          <xsl:attribute name="line" select="if (@line) then @line else 0"/>
          <xsl:text>Package is declared more than once</xsl:text>
        </error>
      </xsl:for-each>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="(node() except errors)|@*"/>
      <xsl:if test="exists($errors) or exists(/object/errors)">
        <errors>
          <xsl:apply-templates select="/object/errors/error"/>
          <xsl:copy-of select="$errors"/>
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
