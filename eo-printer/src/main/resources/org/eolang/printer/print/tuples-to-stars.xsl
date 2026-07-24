<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="tuples-to-stars" version="2.0">
  <!--
  Performs the reverse operation of "/org/eolang/parser/stars-to-tuples.xsl".

  Each "Φ.tuple" layer built by that sheet holds three children: the
  nested tuple (o[1]), the element it appends (o[2]), and a trailing
  "Φ.number" that carries the tuple's length (o[last()]). The element is
  therefore whatever sits strictly between the nested tuple and that
  length marker — selected here as "o[position() != 1 and position() !=
  last()]" rather than a fixed o[2].

  A "| pipe" continuation (§3.14) whose predecessor formation is floated
  up out of the argument slot ("vars-float-up.xsl") leaves a layer with
  only its nested tuple and length marker and no element in between. The
  positional range then selects nothing, so the layer contributes no star
  element — where a fixed o[2] would have taken the length marker for a
  spurious numeric element (#5858).
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@base = 'Φ.tuple' and o[1][starts-with(@base, 'Φ.tuple')]]">
    <xsl:variable name="arg">
      <xsl:apply-templates select="o[position() != 1 and position() != last()]"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:attribute name="star"/>
      <xsl:apply-templates select="o[1]" mode="inner"/>
      <xsl:apply-templates select="$arg" mode="no-as"/>
    </xsl:copy>
  </xsl:template>
  <!--
  An empty tuple is stored as the bare "Φ.tuple.empty" base. Render it as
  the "*" star shorthand with no elements, mirroring how non-empty tuples
  are lowered to stars above.
  -->
  <xsl:template match="o[@base = 'Φ.tuple.empty']">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:attribute name="star"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o" mode="inner">
    <xsl:if test="@base = 'Φ.tuple'">
      <xsl:variable name="arg">
        <xsl:apply-templates select="o[position() != 1 and position() != last()]"/>
      </xsl:variable>
      <xsl:apply-templates select="o[1]" mode="inner"/>
      <xsl:apply-templates select="$arg" mode="no-as"/>
    </xsl:if>
  </xsl:template>
  <xsl:template match="*" mode="no-as">
    <xsl:copy>
      <xsl:copy-of select="@* except @as"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
