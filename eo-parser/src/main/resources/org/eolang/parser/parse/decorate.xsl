<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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

    If only part of FQN is required to be decorated like:
    ```
    +decorate string text

    # Main.
    [] > main
      Q.org.eolang.string.joined.with.some > @
    ```

    It'll be transformed to:
    ```
    +decorate string text

    # Main.
    [] > main
      some. > @
        with.
          joined.
            Q.org.eolang.text
              Q.org.eolang.string
    ```
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="metas" select="/object/metas/meta[head='decorate']"/>
  <xsl:template match="o[@base]">
    <xsl:apply-templates select="." mode="with-base"/>
  </xsl:template>
  <xsl:template match="o[some $p in $metas/part[1] satisfies starts-with(@base, $p)]" mode="with-base">
    <xsl:variable name="meta" select="$metas[part[1]=current()/@base][1]"/>
    <xsl:choose>
      <xsl:when test="exists($meta)">
        <xsl:apply-templates select="." mode="equal">
          <xsl:with-param name="meta" select="$meta"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="." mode="not-equal"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="o" mode="not-equal">
    <xsl:variable name="meta" select="$metas[starts-with(current()/@base, part[1])][1]"/>
    <xsl:variable name="after" select="reverse(tokenize(substring-after(@base, concat($meta/part[1], '.')), '\.'))"/>
    <xsl:apply-templates select="." mode="method">
      <xsl:with-param name="meta" select="$meta"/>
      <xsl:with-param name="methods" select="$after"/>
      <xsl:with-param name="index" select="1"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="o" mode="method">
    <xsl:param name="meta"/>
    <xsl:param name="methods"/>
    <xsl:param name="index"/>
    <xsl:choose>
      <xsl:when test="$index&gt;count($methods)">
        <xsl:variable name="o">
          <xsl:copy>
            <xsl:attribute name="base" select="$meta/part[1]/text()"/>
            <xsl:apply-templates select="@* except (@base|@as|@name)"/>
          </xsl:copy>
        </xsl:variable>
        <xsl:apply-templates select="$o" mode="equal">
          <xsl:with-param name="meta" select="$meta"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="o">
          <xsl:attribute name="base" select="concat('.', $methods[$index])"/>
          <xsl:if test="$index=1">
            <xsl:apply-templates select="@as|@name"/>
          </xsl:if>
          <xsl:apply-templates select="." mode="method">
            <xsl:with-param name="index" select="$index + 1"/>
            <xsl:with-param name="methods" select="$methods"/>
            <xsl:with-param name="meta" select="$meta"/>
          </xsl:apply-templates>
          <xsl:if test="$index=1">
            <xsl:apply-templates select="node()"/>
          </xsl:if>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="o" mode="equal">
    <xsl:param name="meta"/>
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
          <xsl:apply-templates select="node()|(@* except (@as|@name))"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="o">
          <xsl:attribute name="base" select="$meta/part[$index]/text()"/>
          <xsl:if test="$first">
            <xsl:apply-templates select="@as|@name"/>
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
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
