<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="resolve-aliases" version="2.0">
  <!--
  Alias targets are expanded to Φ-rooted FQNs before this stage.
  Here we replace exact alias names in @base, @atom, each @args
  token, @type (preserving the optional '?'), and +also. This stage
  runs before add-default-package.xsl, which only homes what remains.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@base and not(contains(@base, '.'))]">
    <xsl:variable name="object" select="."/>
    <xsl:copy>
      <xsl:attribute name="base">
        <xsl:variable name="meta" select="/object/metas/meta[head='alias' and part[1] = $object/@base]"/>
        <xsl:choose>
          <xsl:when test="$meta">
            <xsl:value-of select="$meta/part[last()]"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$object/@base"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates select="node()|@* except @base"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[@atom and not(contains(@atom, '.'))]">
    <xsl:variable name="object" select="."/>
    <xsl:copy>
      <xsl:attribute name="atom">
        <xsl:variable name="meta" select="/object/metas/meta[head='alias' and part[1] = $object/@atom]"/>
        <xsl:choose>
          <xsl:when test="$meta">
            <xsl:value-of select="$meta/part[last()]"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$object/@atom"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates select="node()|@* except @atom"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="@args">
    <xsl:attribute name="args">
      <xsl:value-of separator=" " select="for $t in tokenize(., ' ') return (/object/metas/meta[head='alias' and part[1]=$t]/part[last()], $t)[1]"/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@type">
    <xsl:variable name="opt" select="ends-with(., '?')"/>
    <xsl:variable name="type" select="if ($opt) then substring(., 1, string-length(.) - 1) else string(.)"/>
    <xsl:attribute name="type" select="concat((/object/metas/meta[head='alias' and part[1]=$type]/part[last()], $type)[1], if ($opt) then '?' else '')"/>
  </xsl:template>
  <xsl:template match="/object/metas/meta[head='also']/(tail|part)">
    <xsl:variable name="meta" select="/object/metas/meta[head='alias' and part[1] = current()/text()]"/>
    <xsl:copy>
      <xsl:choose>
        <xsl:when test="$meta">
          <xsl:value-of select="$meta/part[last()]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="text()"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
