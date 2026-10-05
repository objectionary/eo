<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="_returns" version="3.0">
  <!--
  Here we say what the body of an object returns, as the tables of
  "eo:inference" say it, so that the planting and the rendering ask the
  tables one and the same way. A stylesheet that includes this one opens
  the tables itself, as the variables "eo:provides", "eo:links" and
  "eo:atoms".
  -->
  <!--
  The rows of the table, by the type each one is about. Without the index
  every question about a formation walks the whole table, and the table of
  eo-runtime holds tens of thousands of rows.
  -->
  <xsl:key name="eo:type" match="type" use="@id"/>
  <!-- The atoms the tables know, by locator, for the type an atom gives. -->
  <xsl:key name="eo:atom" match="atom" use="@loc"/>
  <!--
  The types the body of the object with this locator may be, as the tables
  of "eo:inference" say, or none where they say nothing. An object that
  binds nothing but its body behaves as that body, and "eo:inference"
  writes what it behaves as into the "reduced" cell of its row in
  "provides.xml", after chasing the body through all its copies, so that
  cell answers first. A body that is a formation has no row in "links.xml"
  but a row of its own in "provides.xml", so its "reduced" cell answers
  next. Otherwise every link of the body arrives at one type. Whatever the
  answer, an atom counts as the type "atoms.xml" says it gives, and a
  formation counts as the type its "reduced" cell names, if it has one.
  -->
  <xsl:function name="eo:returns" as="xs:string*">
    <xsl:param name="loc" as="xs:string"/>
    <xsl:variable name="reduced" as="xs:string?" select="(key('eo:type', $loc, $eo:provides)[1]/@reduced, key('eo:type', concat($loc, '.φ'), $eo:provides)[1]/@reduced)[1]"/>
    <xsl:sequence select="for $t in (if (exists($reduced)) then $reduced else key('eo:type', concat($loc, '.φ'), $eo:links)/ref/@loc) return string((key('eo:atom', $t, $eo:atoms)/@forma, key('eo:type', $t, $eo:provides)[1]/@reduced, $t)[1])"/>
  </xsl:function>
</xsl:stylesheet>
