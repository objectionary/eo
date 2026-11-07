<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="https://www.eolang.org" exclude-result-prefixes="eo xs" id="add-default-package" version="3.0">
  <!-- A package-resolution stage; its templates form one cohesive module. -->
  <!-- xslint-disable-file too-many-templates -->
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
  <xsl:variable name="package" select="string((/object/metas/meta[head='package']/part[1])[1])"/>
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
  <!-- The short names the "+alias" metas declare. -->
  <xsl:variable name="aliases" select="/object/metas/meta[head='alias']/part[1]"/>
  <!--
  Home every bare name of a type: a return in "@atom", a void type in
  "@type" or a callback list in "@args". A type is a tree such as
  "Φ.pair(A,number)", so the brackets, the commas, a trailing "?" and
  the spaces between "@args" members stay as they are, and each name
  between them is homed on its own. A type variable, a dotted name, a
  special token and an alias name stay as they are. Any other name goes
  to the current package when "local" is true and the package has it, as
  "eo:homed" decides, and to the root "Φ" otherwise.
  -->
  <xsl:function name="eo:typed" as="xs:string">
    <xsl:param name="type" as="xs:string"/>
    <xsl:param name="local" as="xs:boolean"/>
    <xsl:variable name="parts" as="xs:string*">
      <xsl:analyze-string select="$type" regex="[^(),? ]+">
        <xsl:matching-substring>
          <xsl:sequence select="if (matches(., '^[A-F]$') or contains(., '.') or .=$eo:phi or .=$eo:program or .=$eo:rho or .=$eo:empty or .=$eo:xi or .=$eo:bottom or .=$aliases) then string(.) else if ($local) then eo:homed(.) else concat('Φ.', .)"/>
        </xsl:matching-substring>
        <xsl:non-matching-substring>
          <xsl:sequence select="string(.)"/>
        </xsl:non-matching-substring>
      </xsl:analyze-string>
    </xsl:variable>
    <xsl:sequence select="string-join($parts, '')"/>
  </xsl:function>
  <xsl:template match="o[@base]">
    <xsl:apply-templates select="." mode="with-base"/>
  </xsl:template>
  <xsl:template match="o/@atom">
    <xsl:attribute name="atom" select="eo:typed(., true())"/>
  </xsl:template>
  <xsl:template match="@args|@type">
    <xsl:attribute name="{name()}" select="eo:typed(., false())"/>
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
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
