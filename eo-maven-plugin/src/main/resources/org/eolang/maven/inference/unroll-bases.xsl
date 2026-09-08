<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="https://www.eolang.org" exclude-result-prefixes="eo xs" id="unroll-bases" version="2.0">
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
  A rolled base carries one line and one column, the ones of the name the
  chain ends with, and the others were dropped when it was glued. Here
  they are read back off the source, which the <listing> of the same
  document holds in full, so that every step comes out where its own name
  is written: a dot at the column means the step before it ends on the
  same line just to the left of that dot, and a name standing alone on
  its line with a dot after it means the receiver is the block below, at
  its indent. A step whose receiver the source never wrote, such as the
  package an aliased object is taken from, keeps what it was given and
  shares the word before it, since a reader has nothing else to hang it
  on.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="eo:lines" as="xs:string*" select="tokenize(string(/object/listing), '\r?\n')"/>
  <!--
  How far the leading spaces of a line reach.
  -->
  <xsl:function name="eo:indent" as="xs:integer">
    <xsl:param name="text" as="xs:string"/>
    <xsl:sequence select="string-length($text) - string-length(replace($text, '^ +', ''))"/>
  </xsl:function>
  <!--
  The names and dots a line begins with, once its indent is passed.
  -->
  <xsl:function name="eo:run" as="xs:string">
    <xsl:param name="text" as="xs:string"/>
    <xsl:sequence select="replace(substring($text, eo:indent($text) + 1), '[^A-Za-z0-9_.^-].*$', '')"/>
  </xsl:function>
  <!--
  The column just past the last name of what a line begins with.
  -->
  <xsl:function name="eo:edge" as="xs:integer">
    <xsl:param name="text" as="xs:string"/>
    <xsl:sequence select="eo:indent($text) + string-length(replace(eo:run($text), '\.+$', ''))"/>
  </xsl:function>
  <!--
  The column the thing ending just before this one starts at, or -1
  when there is nothing readable to the left.
  -->
  <xsl:function name="eo:spot" as="xs:integer">
    <xsl:param name="text" as="xs:string"/>
    <xsl:param name="edge" as="xs:integer"/>
    <xsl:variable name="head" as="xs:string" select="substring($text, 1, $edge)"/>
    <xsl:variable name="word" as="xs:string" select="replace($head, '.*[^A-Za-z0-9_-]', '')"/>
    <xsl:variable name="start" as="xs:integer" select="if ($edge &lt;= 0 or $edge &gt; string-length($text)) then -1 else if ($word != '') then $edge - string-length($word) else if (substring($head, $edge, 1) = '^') then $edge - 1 else -1"/>
    <xsl:sequence select="if ($start &gt; 0 and substring($text, $start, 1) = '.') then $start - 1 else $start"/>
  </xsl:function>
  <!--
  The line and the column of the receiver of this dispatch, or nothing
  when the source never wrote it.
  -->
  <xsl:function name="eo:place" as="xs:integer*">
    <xsl:param name="o" as="element(o)"/>
    <xsl:variable name="line" as="xs:integer" select="xs:integer(($o/@line, 0)[1])"/>
    <xsl:variable name="pos" as="xs:integer" select="xs:integer(($o/@pos, -1)[1])"/>
    <xsl:variable name="text" as="xs:string" select="($eo:lines[$line], '')[1]"/>
    <xsl:variable name="below" as="xs:string" select="($eo:lines[$line + 1], '')[1]"/>
    <xsl:variable name="tail" as="xs:string" select="tokenize($o/@base, '\.')[last()]"/>
    <xsl:variable name="along" as="xs:boolean" select="$pos &gt;= 0 and $pos &lt; string-length($text) and substring($text, $pos + 1, 1) = '.'"/>
    <xsl:variable name="down" as="xs:boolean" select="$pos = eo:indent($text) and eo:run($text) = concat(if ($tail = 'ρ') then '^' else $tail, '.') and eo:indent($below) &gt; eo:indent($text)"/>
    <xsl:variable name="found" as="xs:integer" select="if ($along) then $line else if ($down) then $line + 1 else -1"/>
    <xsl:variable name="spot" as="xs:integer" select="eo:spot(($eo:lines[$found], '')[1], if ($along) then $pos else eo:edge($below))"/>
    <xsl:sequence select="if ($spot &lt; 0) then () else ($found, $spot)"/>
  </xsl:function>
  <xsl:template match="o[contains(replace(@base, '\.[^.]*$', ''), '.')]">
    <xsl:variable name="dotted" select="starts-with(@base, '.')"/>
    <xsl:variable name="place" as="xs:integer*" select="eo:place(.)"/>
    <xsl:variable name="receiver" as="element()">
      <o base="{replace(@base, '\.[^.]*$', '')}">
        <xsl:choose>
          <xsl:when test="count($place) = 2">
            <xsl:attribute name="line" select="$place[1]"/>
            <xsl:attribute name="pos" select="$place[2]"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of select="@line|@pos"/>
          </xsl:otherwise>
        </xsl:choose>
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
