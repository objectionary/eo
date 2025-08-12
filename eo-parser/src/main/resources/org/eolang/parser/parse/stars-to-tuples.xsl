<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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

    <o base="Φ.org.eolang.tuple">
      <o base="Φ.org.eolang.tuple">
        <o base="Φ.org.eolang.tuple">
          <o base="Φ.org.eolang.tuple.empty"/>
          <o base="1"/>
          <o base="Φ.org.eolang.number">
            <o base="Φ.org.eolang.bytes" hex="">1</o>
          </o>
        </o>
        <o base="2"/>
        <o base="Φ.org.eolang.number">
          <o base="Φ.org.eolang.bytes" hex="">2</o>
        </o>
      </o>
      <o base="3"/>
      <o base="Φ.org.eolang.number">
        <o base="Φ.org.eolang.bytes" hex="">3</o>
      </o>
    </o>
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@star]">
    <xsl:choose>
      <xsl:when test="count(o)&gt;0">
        <xsl:variable name="nested">
          <xsl:element name="o">
            <xsl:attribute name="star"/>
            <xsl:apply-templates select="o[position()!=last()]"/>
          </xsl:element>
        </xsl:variable>
        <xsl:element name="o">
          <xsl:attribute name="base" select="'Φ.org.eolang.tuple'"/>
          <xsl:apply-templates select="@* except (@star | @base)"/>
          <xsl:apply-templates select="$nested"/>
          <xsl:apply-templates select="o[last()]"/>
          <xsl:element name="o">
            <xsl:attribute name="base" select="'Φ.org.eolang.number'"/>
            <xsl:element name="o">
              <xsl:attribute name="base" select="'Φ.org.eolang.bytes'">
              </xsl:attribute>
              <xsl:element name="o">
                <xsl:attribute name="hex"/>
                <xsl:value-of select="count(o)"/>
              </xsl:element>
            </xsl:element>
          </xsl:element>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="o">
          <xsl:attribute name="base" select="'Φ.org.eolang.tuple.empty'"/>
          <xsl:apply-templates select="@* except (@star | @base)"/>
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
