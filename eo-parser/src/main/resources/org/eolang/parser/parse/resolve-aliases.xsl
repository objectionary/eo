<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="resolve-aliases" version="2.0">
  <!--
  Here we go through all objects that are not methods or have
  composite FQN and try to find their references in aliases.
  If we find them, we change their @base attributes.
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
  <!--
  Resolve every type atom a void's union names (R-3.4.8), leaving the
  braces, pipes and the trailing "?" that hold the union together where
  they are.
  -->
  <xsl:template match="@type">
    <xsl:variable name="aliases" select="/object/metas/meta[head='alias']"/>
    <xsl:attribute name="type">
      <xsl:analyze-string select="." regex="[{{}} ?]">
        <xsl:matching-substring>
          <xsl:value-of select="."/>
        </xsl:matching-substring>
        <xsl:non-matching-substring>
          <xsl:variable name="atom" select="."/>
          <xsl:value-of select="($aliases[part[1]=$atom]/part[last()], $atom)[1]"/>
        </xsl:non-matching-substring>
      </xsl:analyze-string>
    </xsl:attribute>
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
