<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="validate-aliases" version="2.0">
  <!--
  Here we add an error with severity 'critical' when one alias name is
  declared more than once, since a name with two targets cannot be resolved
  and would otherwise end up as a space-joined base.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object">
    <xsl:variable name="errors" as="element()*">
      <xsl:for-each select="metas/meta[head='alias']">
        <xsl:variable name="name" select="part[1]"/>
        <xsl:if test="preceding-sibling::meta[head='alias' and part[1]=$name]">
          <error>
            <xsl:attribute name="check" select="'validate-aliases'"/>
            <xsl:attribute name="line" select="if (@line) then @line else 0"/>
            <xsl:attribute name="severity" select="'critical'"/>
            <xsl:value-of select="concat('Alias &quot;', $name, '&quot; is declared more than once')"/>
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
