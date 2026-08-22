<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="unhome-package" version="2.0">
  <!--
  Takes the program's own package back off the references that
  "add-default-package" homed into it, so that "format" prints from the tree
  "parse" wrote instead of walking every source again. The object names of
  "parse" tell a homed reference from one rooted all along, such as the
  "Φ.tuple.empty" inside "+package tuple". Blanks go, the printer lays its own.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:param name="objects" select="''"/>
  <xsl:variable name="package" select="string((/object/metas/meta[head='package']/part[1])[1])"/>
  <xsl:variable name="known" select="tokenize($objects, '\s+')[. != '']"/>
  <xsl:variable name="prefix" select="concat('Φ.', $package, '.')"/>
  <xsl:function name="eo:homed" as="xs:boolean">
    <xsl:param name="base" as="xs:string"/>
    <xsl:sequence select="$package != '' and starts-with($base, $prefix) and concat($package, '.', tokenize(substring-after($base, $prefix), '\.')[1]) = $known"/>
  </xsl:function>
  <xsl:template match="o/@base[eo:homed(.)]">
    <xsl:attribute name="base" select="concat('Φ.', substring-after(., $prefix))"/>
  </xsl:template>
  <xsl:template match="text()[not(normalize-space())][../*]"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
