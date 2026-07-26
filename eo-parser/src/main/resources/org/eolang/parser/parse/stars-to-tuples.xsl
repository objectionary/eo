<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="stars-to-tuples" version="2.0">
  <!--
  Converts such XMIR with @star attributes:

  <o star="" base="tuple">
  <o base="1".../>
  <o base="2".../>
  <o base="3".../>
  </o>

  Into the next one without @star:

  <o base="Φ.tuple">
  <o base="Φ.tuple">
  <o base="Φ.tuple">
  <o base="Φ.tuple.empty"/>
  <o base="1"/>
  <o base="Φ.number">
  <o base="Φ.bytes" hex="">1</o>
  </o>
  </o>
  <o base="2"/>
  <o base="Φ.number">
  <o base="Φ.bytes" hex="">2</o>
  </o>
  </o>
  <o base="3"/>
  <o base="Φ.number">
  <o base="Φ.bytes" hex="">3</o>
  </o>
  </o>
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  A pipe predecessor floated up out of a star tuple (@float-up, set by
  "wrap-applications" for #5848) is not one of the tuple's own elements — the
  "| args" pipe that follows it is. It is excluded from the "Φ.tuple" layers
  and the length count, but still emitted as a passenger child so the next
  sheet "vars-float-up" can hoist its definition and drop the in-place node.
  Counting it would leave a spurious extra layer with a stray "Φ.number" once
  it is dropped (the parse-side mirror of #5858).
  -->
  <xsl:template match="o[@star]">
    <xsl:variable name="elems" select="o[not(@float-up)]"/>
    <xsl:choose>
      <xsl:when test="count($elems)&gt;0">
        <xsl:variable name="nested">
          <xsl:element name="o">
            <xsl:attribute name="star"/>
            <xsl:apply-templates select="@line"/>
            <xsl:apply-templates select="$elems[position()!=last()]"/>
          </xsl:element>
        </xsl:variable>
        <xsl:element name="o">
          <xsl:attribute name="base" select="'Φ.tuple'"/>
          <xsl:apply-templates select="@* except (@star | @base)"/>
          <xsl:apply-templates select="$nested"/>
          <xsl:apply-templates select="$elems[last()]"/>
          <xsl:element name="o">
            <xsl:attribute name="base" select="'Φ.number'"/>
            <xsl:apply-templates select="@line"/>
            <xsl:element name="o">
              <xsl:attribute name="base" select="'Φ.bytes'"/>
              <xsl:apply-templates select="@line"/>
              <xsl:element name="o">
                <xsl:attribute name="hex"/>
                <xsl:apply-templates select="@line"/>
                <xsl:value-of select="count($elems)"/>
              </xsl:element>
            </xsl:element>
          </xsl:element>
          <xsl:apply-templates select="o[@float-up]"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="o">
          <xsl:attribute name="base" select="'Φ.tuple.empty'"/>
          <xsl:apply-templates select="@* except (@star | @base)"/>
          <xsl:apply-templates select="o[@float-up]"/>
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
