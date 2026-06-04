<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="expand-on-error" version="2.0">
  <!--
  Lower `book? > [e] >>` (only-phi with @on-error) into an `on-error` application:

    <o on-error="">
      <o base="∅" name="e"/>
      <o base="Φ.book" name="φ" on-error=""/>
    </o>

  becomes:

    <o base="Φ.on-error" name="φ">
      <o as="α0" base="Φ.book"/>
      <o as="α1">
        <o base="∅" name="e"/>
        <o base="Φ.true" name="φ"/>
      </o>
      <o as="α2" base="Φ.true"/>
    </o>
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@on-error][o[@name='φ' and @on-error]]">
    <xsl:variable name="main" select="o[@name='φ' and @on-error]"/>
    <xsl:variable name="voids" select="o[@base='∅']"/>
    <xsl:element name="o">
      <xsl:attribute name="base" select="'Φ.on-error'"/>
      <xsl:attribute name="name" select="'φ'"/>
      <xsl:if test="@line">
        <xsl:attribute name="line" select="@line"/>
      </xsl:if>
      <xsl:if test="@pos">
        <xsl:attribute name="pos" select="@pos"/>
      </xsl:if>
      <xsl:element name="o">
        <xsl:attribute name="as" select="'α0'"/>
        <xsl:for-each select="$main/@*[name()!='on-error' and name()!='name']">
          <xsl:attribute name="{name()}" select="."/>
        </xsl:for-each>
        <xsl:copy-of select="$main/node()"/>
      </xsl:element>
      <xsl:element name="o">
        <xsl:attribute name="as" select="'α1'"/>
        <xsl:if test="@line">
          <xsl:attribute name="line" select="@line"/>
        </xsl:if>
        <xsl:if test="@pos">
          <xsl:attribute name="pos" select="@pos"/>
        </xsl:if>
        <xsl:for-each select="$voids">
          <xsl:copy-of select="."/>
        </xsl:for-each>
        <xsl:element name="o">
          <xsl:attribute name="base" select="'Φ.true'"/>
          <xsl:attribute name="name" select="'φ'"/>
          <xsl:if test="@line">
            <xsl:attribute name="line" select="@line"/>
          </xsl:if>
          <xsl:if test="@pos">
            <xsl:attribute name="pos" select="@pos"/>
          </xsl:if>
        </xsl:element>
      </xsl:element>
      <xsl:element name="o">
        <xsl:attribute name="as" select="'α2'"/>
        <xsl:attribute name="base" select="'Φ.true'"/>
        <xsl:if test="@line">
          <xsl:attribute name="line" select="@line"/>
        </xsl:if>
        <xsl:if test="@pos">
          <xsl:attribute name="pos" select="@pos"/>
        </xsl:if>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
