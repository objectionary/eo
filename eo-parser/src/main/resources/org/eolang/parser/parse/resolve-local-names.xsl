<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs" id="resolve-local-names" version="2.0">
  <!--
  The "&gt;&gt; foo" file-local handle (§3.10 / §9.2): the parser emits the
  anonymous object with its cactus @name plus a "@local='foo'" marker. We
  build the per-file "handle -&gt; cactus-name" table and rewrite every @base
  equal to a handle into that (reserved) cactus name; a handle declared more
  than once is an error. The "@local" marker is deliberately kept on the
  declaring object so that later passes (in particular the printer, see
  #5563) can recover the readable handle from the otherwise-synthetic cactus
  name instead of printing a placeholder like "vL_P".
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:key name="handle" match="o[@local]" use="@local"/>
  <xsl:template match="o[@base and key('handle', @base)]">
    <xsl:copy>
      <xsl:attribute name="base" select="key('handle', @base)[1]/@name"/>
      <xsl:apply-templates select="@* except @base"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="/object">
    <xsl:copy>
      <xsl:apply-templates select="(node() except errors)|@*"/>
      <xsl:variable name="errors" as="element()*">
        <xsl:for-each-group select="//o[@local]" group-by="@local">
          <xsl:if test="count(current-group()) &gt; 1">
            <xsl:element name="error">
              <xsl:attribute name="check" select="'resolve-local-names'"/>
              <xsl:attribute name="line" select="if (current-group()[2]/@line) then current-group()[2]/@line else 0"/>
              <xsl:attribute name="severity" select="'error'"/>
              <xsl:text>duplicate local name '</xsl:text>
              <xsl:value-of select="current-grouping-key()"/>
              <xsl:text>'</xsl:text>
            </xsl:element>
          </xsl:if>
        </xsl:for-each-group>
      </xsl:variable>
      <xsl:if test="not(empty($errors)) or exists(/object/errors)">
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
