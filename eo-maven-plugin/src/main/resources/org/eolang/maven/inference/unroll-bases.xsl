<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="unroll-bases" version="2.0">
  <!--
  Here we split every composite @base into one object per dispatch step,
  which is exactly what "roll-bases.xsl" of the parser glued together:
  - <o base="ξ.x.next.foo"/>  =>  <o base=".foo">
  <o base=".next">
  <o base="ξ.x"/>
  </o>
  </o>
  - <o base="Φ.io.stdout"/>  =>  <o base=".stdout">
  <o base="Φ.io"/>
  </o>
  The head keeps the first name attached to it, so a base with a single
  dot ("ξ.x", "Φ.io", ".foo") is a reference and stays as it is. Only
  the names after that one become dispatches, because a type mistake can
  only be found where an attribute is taken from an object, and that is
  what one dispatch is.
  Arguments belong to the outermost dispatch ("x.next.foo 5" applies the
  5 to "foo", not to "x"), so they stay where they were. The receiver of
  a base that already starts with a dot is its first child, so that child
  moves down into the shorter base together with it.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[contains(replace(@base, '\.[^.]*$', ''), '.')]">
    <xsl:variable name="dotted" select="starts-with(@base, '.')"/>
    <xsl:variable name="receiver" as="element()">
      <o base="{replace(@base, '\.[^.]*$', '')}">
        <xsl:copy-of select="@line|@pos"/>
        <xsl:if test="$dotted">
          <xsl:copy-of select="o[1]"/>
        </xsl:if>
      </o>
    </xsl:variable>
    <o>
      <xsl:apply-templates select="@* except @base"/>
      <xsl:attribute name="base" select="concat('.', tokenize(@base, '\.')[last()])"/>
      <xsl:apply-templates select="$receiver"/>
      <xsl:apply-templates select="o[not($dotted) or position() &gt; 1]"/>
    </o>
  </xsl:template>
  <!-- Default copying -->
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
