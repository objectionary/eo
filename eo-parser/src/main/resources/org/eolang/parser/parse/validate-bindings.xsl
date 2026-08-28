<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="validate-bindings" version="2.0">
  <!--
  Here we add an error with severity 'critical' when two arguments of one
  application are bound to the same slot, since they cannot both be written
  into a single attribute.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object">
    <xsl:variable name="errors" as="element()*">
      <xsl:for-each select=".//o[@as]">
        <xsl:variable name="as" select="@as"/>
        <xsl:if test="preceding-sibling::o[@as=$as]">
          <error>
            <xsl:attribute name="check" select="'validate-bindings'"/>
            <xsl:attribute name="line" select="if (@line) then @line else 0"/>
            <xsl:attribute name="severity" select="'critical'"/>
            <xsl:if test="@pos">
              <xsl:attribute name="pos" select="@pos"/>
            </xsl:if>
            <xsl:value-of select="concat('Argument is bound to &quot;', $as, '&quot;, which is already taken by another argument')"/>
          </error>
        </xsl:if>
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
