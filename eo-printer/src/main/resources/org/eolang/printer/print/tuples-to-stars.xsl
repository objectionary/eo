<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="tuples-to-stars" version="3.0">
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
  <!--
  How many elements a chain of layers holds, counted by the layers
  themselves rather than read out of the length each one carries.
  -->
  <xsl:function name="eo:depth" as="xs:integer">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="if ($o/@base = 'Φ.tuple.empty') then 0 else eo:depth($o/o[1]) + 1"/>
  </xsl:function>
  <!--
  Whether the last child of a layer is a length marker that agrees with
  the elements beside it. This sheet runs right after "StUnhex" (see
  "Xmir"), which folds the bytes of a number into the text of the node, so
  the length is readable here and is read. Before that fold a number the
  author wrote is bytes, and a length nobody can read is left alone rather
  than guessed at.
  -->
  <xsl:function name="eo:fits" as="xs:boolean">
    <xsl:param name="marker" as="element()?"/>
    <xsl:param name="length" as="xs:integer"/>
    <xsl:sequence select="exists($marker) and $marker/@base = 'Φ.number' and (not(matches(normalize-space($marker), '^[0-9]+$')) or xs:integer(normalize-space($marker)) = $length)"/>
  </xsl:function>
  <!--
  Whether a layer is one "stars-to-tuples" built, which is the only shape
  this sheet knows how to read back (#9165). That sheet makes a nested
  tuple, one element and the length of the two together, all bound by
  position; the element is gone when a pipe predecessor was floated out of
  the slot, which leaves the layer with two children. A "tuple" the author
  wrote by hand answers to none of that, and rewriting it as a star drops
  the arguments the star has no room for and recomputes the length, which
  the re-parse accepts without a word.
  -->
  <xsl:function name="eo:star-layer" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="$o/@base = 'Φ.tuple' and count($o/o) = (2, 3) and empty($o/o/@as[not(matches(., '^α[0-9]+$'))]) and ($o/o[1]/@base = 'Φ.tuple.empty' or ($o/o[1]/@base = 'Φ.tuple' and eo:star-layer($o/o[1]))) and eo:fits($o/o[last()], eo:depth($o))"/>
  </xsl:function>
  <xsl:template match="o[eo:star-layer(.)]">
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

  The same base is what "resolve-aliases.xsl" puts on an ordinary
  reference whenever the program declares "+alias tuple.empty", so a node
  carrying it is not necessarily a "*" in the source — it may just be a
  name the author wrote that happens to resolve to this base. Lowering it
  to a star anyway would print the wrong shorthand and leave the alias
  meta dangling with nothing left referring to it. Skip a node whose base
  is the target of such an alias, leaving it for "restore-aliases.xsl" to
  print back under its declared short name instead.
  -->
  <xsl:template match="o[@base = 'Φ.tuple.empty' and not(/object/metas/meta[head = 'alias' and part[last()] = 'Φ.tuple.empty'])]">
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
