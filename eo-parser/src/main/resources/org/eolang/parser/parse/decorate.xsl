<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="decorate" version="2.0">
  <!--
    Decorate objects. Such EO code:
    ```
    +decorate string text foo

    # Main.
    [] > main
      "Hello" > @
    ```

    will be transformed into:
    ```
    +decorate string text foo

    # Main.
    [] > main
      foo > @
        text
          "Hello"
    ```
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="metas" select="/program/metas/meta[head='decorate']"/>
  <xsl:template match="o[@base and $metas/part[1]=@base]">
    <xsl:variable name="meta" select="$metas[part[1]=current()/@base][1]"/>
    <xsl:choose>
      <xsl:when test="count($meta/part)&lt;=1">
        <xsl:element name="o">
          <xsl:apply-templates select="node()|@*"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="." mode="decorated">
          <xsl:with-param name="index" select="count($meta/part)"/>
          <xsl:with-param name="meta" select="$meta"/>
          <xsl:with-param name="first" select="true()"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="o" mode="decorated">
    <xsl:param name="index"/>
    <xsl:param name="meta"/>
    <xsl:param name="first"/>
    <xsl:choose>
      <xsl:when test="$index=1">
        <xsl:element name="o">
          <xsl:apply-templates select="node()|@*[name()!='as' and name()!='name']"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="o">
          <xsl:attribute name="base" select="$meta/part[$index]/text()"/>
          <xsl:if test="$first">
            <xsl:apply-templates select="@name"/>
            <xsl:apply-templates select="@as"/>
          </xsl:if>
          <xsl:apply-templates select="." mode="decorated">
            <xsl:with-param name="index" select="$index - 1"/>
            <xsl:with-param name="meta" select="$meta"/>
            <xsl:with-param name="first" select="false()"/>
          </xsl:apply-templates>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
