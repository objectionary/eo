<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="unhome-package" version="2.0">
  <!--
  Takes the program's own package back off the references that
  "add-default-package" homed into it, so that "format" can print from
  the tree "parse" already wrote instead of walking every source again.

  The "parse" goal hands that sheet the qualified names of the local
  package objects, so a bare reference to one of them comes out homed:
  `length` in a program declaring `+package input` becomes
  `Φ.input.length`. The printer must not put that prefix into a source
  that never carried it, so it comes off again here, and this sheet is
  given the very same list of names so that it undoes exactly what was
  done and nothing else.

  The list matters. Without it every `Φ.<package>.` prefix would look
  like homing, and `Φ.tuple.empty` inside `+package tuple` would lose
  its root: that is the global `empty` attribute of the root `tuple`
  object, not a package-mate, and stripping it turns `*` into `empty`.
  Only a name the list knows is a local package object was homed, so
  only such a name is taken back.

  Only "base" is touched. "add-default-package" also homes a bare
  "atom" signature, but a signature is printed rooted either way
  (see "eo:signature" in "to-eo-tree"), so taking the package off one
  would rewrite it rather than restore it.

  A reference written out in full ("input.length" inside package
  "input") rolls into the very same base as the bare one and is taken
  back too, so both spellings settle on the bare form. They name one
  and the same object, because the prefix is only ever added when the
  qualified name is a local package object, so this is a matter of
  spelling rather than of meaning, and the bare form is the one the
  canonical layout keeps.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- Space separated qualified names of the local package objects, as "parse" knows them. -->
  <xsl:param name="objects" select="''"/>
  <!-- The package of the current program, empty when there is no "+package" meta. -->
  <xsl:variable name="package" select="string((/object/metas/meta[head='package']/part[1])[1])"/>
  <!-- Those qualified names as a sequence. -->
  <xsl:variable name="known" select="tokenize($objects, '\s+')[. != '']"/>
  <!-- The prefix that homing into the current package leaves behind. -->
  <xsl:variable name="prefix" select="concat('Φ.', $package, '.')"/>
  <!-- Whether a base carries a prefix that "add-default-package" put there. -->
  <xsl:function name="eo:homed" as="xs:boolean">
    <xsl:param name="base" as="xs:string"/>
    <xsl:variable name="rest" select="substring-after($base, $prefix)"/>
    <xsl:sequence select="$package != '' and starts-with($base, $prefix) and concat($package, '.', tokenize($rest, '\.')[1]) = $known"/>
  </xsl:function>
  <xsl:template match="o/@base[eo:homed(.)]">
    <xsl:attribute name="base" select="concat('Φ.', substring-after(., $prefix))"/>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
