<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="auto-phi-formation-restruct" version="2.0">
  <!--
    Here, we're taking all the auto-phi formations, and transform from this structure:
    ```
    <o line="10" name="ap🌵108" pos="8">
      <o base="∅" line="10" name="i" pos="24"/>
      <o base="ξ.ρ.m.get.eq" line="10" name="φ" pos="8">
        <o as="α0" base="ξ.ρ.m.get.eq" line="10" pos="13">
          <o as="α0" base="Φ.org.eolang.number" line="10" pos="17".../>
          <o as="α1" base="ξ.i" line="10"/>
        </o>
      </o>
    </o>
    ```
    to this:
    ```
    <o line="10" name="ap🌵108" pos="8">
      <o base="∅" line="10" name="i" pos="24"/>
      <o base="ξ.ρ.m.get.eq" line="10" name="φ" pos="8">
        <o as="α0" base="Φ.org.eolang.number" line="10" pos="17".../>
      </o>
    </o>
    ```
  -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[@base and @name='φ' and ancestor::o[contains(@name,'ap🌵')]]">
    <xsl:variable name="inner" select="descendant::o[@base = current()/@base][1]"/>
    <xsl:choose>
      <xsl:when test="$inner">
        <xsl:apply-templates select="$inner" mode="promote">
          <xsl:with-param name="outer-name" select="@name"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="o" mode="promote">
    <xsl:param name="outer-name"/>
    <xsl:copy>
      <xsl:copy-of select="@*[name()!='name']"/>
      <xsl:attribute name="name">
        <xsl:value-of select="$outer-name"/>
      </xsl:attribute>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <!-- Remove redundant ξ.X for void attributes, located inside the auto-phi formation -->
  <xsl:template match="o[starts-with(@base,'ξ.') and ancestor::o[contains(@name,'ap🌵')][descendant::o[@base='∅' and @name=substring-after(current()/@base,'ξ.')]]]"/>
</xsl:stylesheet>
