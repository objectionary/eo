<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="resolve-aliases" version="3.0">
  <!--
  Here we go through all objects that are not methods or have
  composite FQN and try to find their references in aliases.
  If we find them, we change their @base attributes.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- The "+alias" metas of the program. -->
  <xsl:variable name="aliases" select="/object/metas/meta[head='alias']"/>
  <!--
  Resolve every aliased name of a type: a return in "@atom", a void type
  in "@type" or a callback list in "@args". A type is a tree such as
  "foo(A,bar)|baz", so the brackets, the commas, the "|", a trailing "?"
  and the spaces between "@args" members stay as they are, and each name
  between them that an alias declares becomes the alias target.
  -->
  <xsl:function name="eo:resolved" as="xs:string">
    <xsl:param name="type" as="xs:string"/>
    <xsl:variable name="parts" as="xs:string*">
      <xsl:analyze-string select="$type" regex="[^(),|? ]+">
        <xsl:matching-substring>
          <xsl:variable name="name" select="."/>
          <xsl:sequence select="string(($aliases[part[1] = $name]/part[last()], $name)[1])"/>
        </xsl:matching-substring>
        <xsl:non-matching-substring>
          <xsl:sequence select="string(.)"/>
        </xsl:non-matching-substring>
      </xsl:analyze-string>
    </xsl:variable>
    <xsl:sequence select="string-join($parts, '')"/>
  </xsl:function>
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
  <xsl:template match="o/@atom|@args|@type">
    <xsl:attribute name="{name()}" select="eo:resolved(.)"/>
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
