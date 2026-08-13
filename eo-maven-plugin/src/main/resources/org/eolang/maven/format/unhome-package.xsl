<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="unhome-package" version="2.0">
  <!--
  Takes the program's own package back off the references that
  "add-default-package" homed into it, so that "format" can print from the
  tree "parse" wrote instead of walking every source a second time.

  It is given the same object names "parse" was given, so it undoes exactly
  what was done. Without them every "Φ.<package>." prefix would look homed,
  and "Φ.tuple.empty" inside "+package tuple" would lose its root: that is
  the global "empty" of the root "tuple", and stripping it turns "*" into
  "empty". Only "base" is touched, since a signature prints rooted anyway.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- Space separated qualified names of the local package objects. -->
  <xsl:param name="objects" select="''"/>
  <xsl:variable name="package" select="string((/object/metas/meta[head='package']/part[1])[1])"/>
  <xsl:variable name="known" select="tokenize($objects, '\s+')[. != '']"/>
  <xsl:variable name="prefix" select="concat('Φ.', $package, '.')"/>
  <!-- Whether a base carries a prefix that "add-default-package" put there. -->
  <xsl:function name="eo:homed" as="xs:boolean">
    <xsl:param name="base" as="xs:string"/>
    <xsl:sequence select="$package != '' and starts-with($base, $prefix) and concat($package, '.', tokenize(substring-after($base, $prefix), '\.')[1]) = $known"/>
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
