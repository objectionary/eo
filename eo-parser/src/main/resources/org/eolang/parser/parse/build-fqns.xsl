<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="build-fqns" version="2.0">
  <!--
  Here we go through all objects and find what their @base
  are referring to. If we find the object they refer to,
  we add either $ object (if target object in the same scope)
  or necessary amount of '^.' objects. Those objects
  which are skipped after this transformation
  are not visible in the current scope. Maybe they are
  global or just a mistake.

  We must skip objects that refer to
  "bytes", "string" or "number" if such objects are inside the
  "org.eolang.bytes", "org.eolang.string" or "org.eolang.bytes".
  Such a reference would be misleading: instead of referring to the
  global, for example, "org.eolang.bytes" they will lead to local "bytes"
  defined in this particular file.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:variable name="this">
    <xsl:element name="o">
      <xsl:attribute name="base" select="'ξ'"/>
    </xsl:element>
  </xsl:variable>
  <!-- Build recursive objects chain from package if exists -->
  <xsl:template match="o" mode="recursive-package">
    <xsl:param name="pkg"/>
    <xsl:choose>
      <xsl:when test="$pkg='' or empty($pkg)">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="last">
          <xsl:choose>
            <xsl:when test="contains($pkg, '.')">
              <xsl:value-of select="substring-before($pkg, '.')"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$pkg"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:variable name="elem">
          <xsl:element name="o">
            <xsl:attribute name="base">
              <xsl:text>.</xsl:text>
              <xsl:value-of select="$last"/>
            </xsl:attribute>
            <xsl:copy-of select="."/>
          </xsl:element>
        </xsl:variable>
        <xsl:apply-templates select="$elem" mode="recursive-package">
          <xsl:with-param name="pkg" select="substring-after($pkg, '.')"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- Copy given element and append package if exists -->
  <xsl:template match="o" mode="with-package">
    <xsl:param name="parent"/>
    <xsl:param name="find"/>
    <xsl:choose>
      <xsl:when test="$parent/o[@name=$find]">
        <xsl:variable name="start">
          <xsl:element name="o">
            <xsl:attribute name="base" select="'Φ'"/>
          </xsl:element>
        </xsl:variable>
        <xsl:apply-templates select="." mode="to-method">
          <xsl:with-param name="of">
            <xsl:apply-templates select="$start" mode="recursive-package">
              <xsl:with-param name="pkg" select="/object/metas/meta[head='package']/part[1]/text()"/>
            </xsl:apply-templates>
          </xsl:with-param>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$find='φ'">
          <xsl:message terminate="yes">
            <xsl:text>The </xsl:text>
            <xsl:value-of select="$find"/>
            <xsl:text> object is used, but absent in self or parents scope</xsl:text>
          </xsl:message>
        </xsl:if>
        <xsl:copy>
          <xsl:apply-templates select="node()|@*"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- Convert given object to method -->
  <xsl:template match="o" mode="to-method">
    <xsl:param name="of"/>
    <xsl:element name="o">
      <xsl:attribute name="base">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="@base"/>
      </xsl:attribute>
      <xsl:apply-templates select="@* except @base"/>
      <xsl:copy-of select="$of"/>
      <xsl:apply-templates select="o"/>
    </xsl:element>
  </xsl:template>
  <!-- ADD ^. TO GIVEN OBJECT (OR NOT) -->
  <xsl:template match="o" mode="with-rho">
    <xsl:param name="rhos"/>
    <xsl:param name="current"/>
    <xsl:choose>
      <!-- No need to add rho - we're in the current scope, but in application -->
      <xsl:when test="$rhos=0">
        <xsl:apply-templates select="." mode="to-method">
          <xsl:with-param name="of" select="$this"/>
        </xsl:apply-templates>
      </xsl:when>
      <!-- We're in the end of recursion -->
      <xsl:when test="$rhos=1">
        <xsl:apply-templates select="." mode="to-method">
          <xsl:with-param name="of" select="$current"/>
        </xsl:apply-templates>
      </xsl:when>
      <!-- Next iteration -->
      <xsl:otherwise>
        <xsl:apply-templates mode="with-rho" select=".">
          <xsl:with-param name="rhos" select="$rhos - 1"/>
          <xsl:with-param name="current">
            <xsl:element name="o">
              <xsl:attribute name="base" select="'.ρ'"/>
              <xsl:copy-of select="$current"/>
            </xsl:element>
          </xsl:with-param>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- BUILD FQN PATH TO OBJECT -->
  <xsl:template match="o" mode="fqn">
    <xsl:param name="rhos"/>
    <xsl:param name="self"/>
    <xsl:param name="find"/>
    <xsl:variable name="parent" select="parent::*"/>
    <xsl:choose>
      <!-- last frontier -->
      <xsl:when test="$parent[name()='object']">
        <xsl:apply-templates select="$self" mode="with-package">
          <xsl:with-param name="find" select="$find"/>
          <xsl:with-param name="parent" select="$parent"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:when test="eo:abstract($parent)">
        <xsl:choose>
          <!-- Found reference in some abstract object above -->
          <xsl:when test="$parent/o[@name=$find]">
            <xsl:apply-templates select="$self" mode="with-rho">
              <xsl:with-param name="rhos" select="$rhos"/>
              <xsl:with-param name="current">
                <xsl:element name="o">
                  <xsl:attribute name="base" select="'.ρ'"/>
                  <xsl:copy-of select="$this"/>
                </xsl:element>
              </xsl:with-param>
            </xsl:apply-templates>
          </xsl:when>
          <!-- No reference - go upper -->
          <xsl:otherwise>
            <xsl:apply-templates select="$parent" mode="fqn">
              <xsl:with-param name="self" select="$self"/>
              <xsl:with-param name="rhos" select="$rhos + 1"/>
              <xsl:with-param name="find" select="$find"/>
            </xsl:apply-templates>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <!-- Parent is not abstract (application) - try to find reference upper -->
      <xsl:otherwise>
        <xsl:apply-templates select="$parent" mode="fqn">
          <xsl:with-param name="self" select="$self"/>
          <xsl:with-param name="find" select="$find"/>
          <xsl:with-param name="rhos" select="$rhos"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- ENTRY POINT -->
  <xsl:template match="o[@base]">
    <xsl:apply-templates select="." mode="with-base"/>
  </xsl:template>
  <xsl:template match="o[not(contains(@base, '.'))]" mode="with-base">
    <xsl:apply-templates select="." mode="no-dots"/>
  </xsl:template>
  <xsl:template match="o[@base='ρ']" mode="no-dots">
    <xsl:element name="o">
      <xsl:apply-templates select="@* except @base"/>
      <xsl:attribute name="base" select="'.ρ'"/>
      <xsl:element name="o">
        <xsl:attribute name="line" select="@line"/>
        <xsl:attribute name="pos" select="@pos - 1"/>
        <xsl:attribute name="base" select="'ξ'"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template match="o[@base!='ξ' and @base!='ρ' and @base!=$eo:empty]" mode="no-dots">
    <xsl:variable name="base" select="./@base"/>
    <xsl:apply-templates select="." mode="fqn">
      <xsl:with-param name="self" select="."/>
      <xsl:with-param name="find" select="$base"/>
      <xsl:with-param name="rhos" select="0"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
