<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="https://www.eolang.org" id="add-default-package" version="2.0">
  <!--
  Here we go through all objects that are not:
  1. methods (starts with .)
  2. @, Q, ^ or $
  3. mentioned in aliases

  and add default package to them.

  We ignore objects that are present in aliases with their exact
  names. For example, this object 'hello' won't be touched, we
  won't think that it belongs to org.eolang package:

  +alias hello

  # No comment.
  [] > app
  hello > @

  The default package is the root "Φ", unless the program has a
  "+package" meta AND the bare name belongs to an object of the
  current package. In the latter case a bare reference is resolved to
  the current package, so that objects from the same package don't
  have to be aliased manually. For example, this program:

  +package foo

  [] > x
  bar 42 > @

  is compiled as if there was a "+alias foo.bar" meta (i.e. 'bar'
  becomes 'Φ.foo.bar'), but only when 'foo.bar' is one of the local
  package objects. The qualified names (i.e. "package.name") of all
  local package objects are provided through the "objects" parameter
  (space separated), which the eo-maven-plugin fills in. A bare name is
  homed into the current package only when "package.name" is in that
  list; otherwise it is treated as a global and homed into the root
  "Φ" (the default behaviour). Thus a bare global like 'seq' (which
  lives at "Φ.seq", and is not a local package object) is left
  untouched, and a name that exists only in some other package is not
  wrongly homed into the current one. When the "objects" parameter is
  empty (for example, when the parser is used stand-alone), every bare
  reference falls back to the root "Φ".
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_specials.xsl"/>
  <!--
  Space separated list of the qualified names ("package.name") of all
  objects that belong to a package and are known to the compiler. Used
  to decide whether a bare reference belongs to the current package or
  is a global.
  -->
  <xsl:param name="objects" as="xs:string" select="''"/>
  <!-- The package of the current program (empty if there is no "+package" meta). -->
  <xsl:variable name="package" select="string(/object/metas/meta[head='package']/part[1])"/>
  <!-- Qualified names of all local package objects as a sequence. -->
  <xsl:variable name="known" select="tokenize($objects, '\s+')[. != '']"/>
  <!--
  Resolve a bare name to its fully qualified name. If the current
  program has a package and "package.name" is a local package object,
  the name is homed into the package; otherwise it goes to the root "Φ".
  -->
  <xsl:function name="eo:homed" as="xs:string">
    <xsl:param name="name" as="xs:string"/>
    <xsl:variable name="local" select="concat($package, '.', $name)"/>
    <xsl:sequence select="concat('Φ.', if ($package != '' and $local = $known) then $local else $name)"/>
  </xsl:function>
  <xsl:template match="o[@base]">
    <xsl:apply-templates select="." mode="with-base"/>
  </xsl:template>
  <xsl:template match="o[@atom][not(matches(@atom, '^[A-F]$'))]">
    <xsl:apply-templates select="." mode="with-atom"/>
  </xsl:template>
  <xsl:template match="@args">
    <xsl:attribute name="args">
      <xsl:value-of separator=" " select="for $t in tokenize(., ' ') return if (matches($t, '^[A-F]$') or contains($t, '.') or $t=$eo:phi or $t=$eo:program or $t=$eo:rho or $t=$eo:empty or $t=$eo:xi or $t=$eo:bottom or $t=/object/metas/meta[head='alias']/part[1]) then $t else concat('Φ.', $t)"/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@type">
    <xsl:variable name="opt" select="ends-with(., '?')"/>
    <xsl:variable name="t" select="if ($opt) then substring(., 1, string-length(.) - 1) else string(.)"/>
    <xsl:variable name="homed" select="if (matches($t, '^[A-F]$') or contains($t, '.') or $t=$eo:phi or $t=$eo:program or $t=$eo:rho or $t=$eo:empty or $t=$eo:xi or $t=$eo:bottom or $t=/object/metas/meta[head='alias']/part[1]) then $t else concat('Φ.', $t)"/>
    <xsl:attribute name="type" select="if ($opt) then concat($homed, '?') else $homed"/>
  </xsl:template>
  <xsl:template match="/object/metas/meta[head='also']/(tail|part)">
    <xsl:apply-templates select="." mode="meta"/>
  </xsl:template>
  <xsl:template match="*[not(contains(text(), ' '))]" mode="meta">
    <xsl:copy>
      <xsl:choose>
        <xsl:when test="not(starts-with(text(), 'Φ.'))">
          <xsl:text>Φ.</xsl:text>
          <xsl:value-of select="text()"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="text()"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[not(contains(@base, '.'))]" mode="with-base">
    <xsl:apply-templates select="." mode="no-dots"/>
  </xsl:template>
  <xsl:template match="o[@base!=$eo:phi and @base!=$eo:program and @base!=$eo:rho and @base!=$eo:empty and @base!=$eo:xi and @base!=$eo:bottom]" mode="no-dots">
    <xsl:apply-templates select="." mode="no-specials"/>
  </xsl:template>
  <xsl:template match="o[not(@base=/object/metas/meta[head='alias']/part[1])]" mode="no-specials">
    <xsl:copy>
      <xsl:attribute name="base" select="eo:homed(@base)"/>
      <xsl:apply-templates select="node()|@* except @base"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o[not(contains(@atom, '.'))]" mode="with-atom">
    <xsl:apply-templates select="." mode="atom-no-dots"/>
  </xsl:template>
  <xsl:template match="o[@atom!=$eo:phi and @atom!=$eo:program and @atom!=$eo:rho and @atom!=$eo:empty and @atom!=$eo:xi and @atom!=$eo:bottom]" mode="atom-no-dots">
    <xsl:apply-templates select="." mode="atom-no-specials"/>
  </xsl:template>
  <xsl:template match="o[not(@atom=/object/metas/meta[head='alias']/part[1])]" mode="atom-no-specials">
    <xsl:copy>
      <xsl:attribute name="atom" select="eo:homed(@atom)"/>
      <xsl:apply-templates select="node()|@* except @atom"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
