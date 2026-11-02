<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="stars-to-tuples" version="3.0">
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

  The layers are built from the innermost one outwards, each wrapping the one
  before it, rather than by a template that calls itself once per element. The
  recursive shape cost one Saxon frame per element and died with "too many
  nested apply-templates calls" on a tuple of about 500, which no caller could
  report, and it re-processed every element once per layer on the way down
  (#9300).
  -->
  <xsl:template match="o[@star]">
    <xsl:variable name="star" select="."/>
    <xsl:variable name="elems" select="o[not(@float-up)]"/>
    <xsl:choose>
      <xsl:when test="count($elems)&gt;0">
        <xsl:variable name="nested" as="element(o)">
          <xsl:iterate select="$elems[position()!=last()]">
            <xsl:param name="layer" as="element(o)">
              <o>
                <xsl:attribute name="base" select="'Φ.tuple.empty'"/>
                <xsl:apply-templates select="$star/(@line | @pos)"/>
              </o>
            </xsl:param>
            <xsl:param name="index" select="0"/>
            <xsl:on-completion select="$layer"/>
            <xsl:next-iteration>
              <xsl:with-param name="layer" as="element(o)">
                <o>
                  <xsl:attribute name="base" select="'Φ.tuple'"/>
                  <xsl:apply-templates select="$star/(@line | @pos)"/>
                  <xsl:sequence select="$layer"/>
                  <xsl:apply-templates select="."/>
                  <xsl:call-template name="length">
                    <xsl:with-param name="star" select="$star"/>
                    <xsl:with-param name="value" select="$index + 1"/>
                  </xsl:call-template>
                </o>
              </xsl:with-param>
              <xsl:with-param name="index" select="$index + 1"/>
            </xsl:next-iteration>
          </xsl:iterate>
        </xsl:variable>
        <o>
          <xsl:attribute name="base" select="'Φ.tuple'"/>
          <xsl:apply-templates select="@* except (@star | @base)"/>
          <xsl:sequence select="$nested"/>
          <xsl:apply-templates select="$elems[last()]"/>
          <xsl:call-template name="length">
            <xsl:with-param name="star" select="$star"/>
            <xsl:with-param name="value" select="count($elems)"/>
          </xsl:call-template>
          <xsl:apply-templates select="o[@float-up]"/>
        </o>
      </xsl:when>
      <xsl:otherwise>
        <o>
          <xsl:attribute name="base" select="'Φ.tuple.empty'"/>
          <xsl:apply-templates select="@* except (@star | @base)"/>
          <xsl:apply-templates select="o[@float-up]"/>
        </o>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!--
  The length a "Φ.tuple" layer carries, as the number of elements under it.
  -->
  <xsl:template name="length">
    <xsl:param name="star" as="element(o)"/>
    <xsl:param name="value"/>
    <o>
      <xsl:attribute name="base" select="'Φ.number'"/>
      <xsl:apply-templates select="$star/(@line | @pos)"/>
      <o>
        <xsl:attribute name="base" select="'Φ.bytes'"/>
        <xsl:apply-templates select="$star/(@line | @pos)"/>
        <o>
          <xsl:attribute name="hex"/>
          <xsl:apply-templates select="$star/(@line | @pos)"/>
          <xsl:value-of select="$value"/>
        </o>
      </o>
    </o>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
