<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="_returns" version="3.0">
  <!--
  Here we say what the body of an object returns, as the tables of
  "eo:inference" say it. The rendering asks this once, when the root of an
  entry is data the atom would return bare, and the planting asks it when it
  decides on a row, and the question is kept apart from the rest of both,
  since it is all about how the tables are laid out. A stylesheet that
  includes this one opens the tables itself, as the variables "eo:facts",
  for "facts.xml", and "eo:answers", for "answers.xml".
  -->
  <!--
  The rows of "answers.xml", by the place each one is about, and the
  formations of "facts.xml", by locator. Without the index every question
  about a place walks the whole table, and the table of eo-runtime holds
  tens of thousands of rows.
  -->
  <xsl:key name="eo:row" match="answers/o" use="@loc"/>
  <xsl:key name="eo:type" match="objects/type" use="@id"/>
  <!--
  The labels the copies of a place say it is, from its row of
  "answers.xml" or from the "reduced" of that row. A label stands right
  under the row when every copy says the same, or inside one "set" per
  group of copies otherwise, and all of them are taken. A "terminator", an
  "open" void and a failing read, "fails", are left out: each is a gap
  next to the names, and none of them says what the place is when it is
  something.
  -->
  <xsl:function name="eo:labels" as="element()*">
    <xsl:param name="row" as="element()*"/>
    <xsl:sequence select="$row/(*[not(self::set or self::reduced)] | set/*)[not(self::terminator or self::open or self::fails)]"/>
  </xsl:function>
  <!--
  The types the body of the object with this locator may be, as the tables
  of "eo:inference" say, or none where they say nothing. They are the
  objects the row of the "φ" of the object names, while a datum and a place
  the engine cannot tell name no object. An atom among them counts as the
  type it "returns" in "facts.xml", and a formation counts as what the
  "reduced" of its own row names other than itself, when it names anything
  else.
  -->
  <xsl:function name="eo:returns" as="xs:string*">
    <xsl:param name="loc" as="xs:string"/>
    <xsl:sequence select="distinct-values(for $name in distinct-values(eo:labels(key('eo:row', concat($loc, '.φ'), $eo:answers))/self::is/@loc) return eo:behaves($name))"/>
  </xsl:function>
  <!--
  What the object with this locator counts as when a body is it: the type
  an atom returns, what a formation reduces to other than itself, or the
  object itself.
  -->
  <xsl:function name="eo:behaves" as="xs:string+">
    <xsl:param name="name" as="xs:string"/>
    <xsl:variable name="atom" as="attribute()?" select="key('eo:type', $name, $eo:facts)[1]/@returns"/>
    <xsl:variable name="reduced" as="xs:string*" select="eo:labels(key('eo:row', $name, $eo:answers)/reduced)/self::is/@loc[. != $name] ! string(.)"/>
    <xsl:sequence select="if (exists($atom)) then string($atom) else if (exists($reduced)) then $reduced else $name"/>
  </xsl:function>
</xsl:stylesheet>
