<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="planting" version="2.0">
  <!--
  Here we find every formation a box may be planted on. A box goes onto
  a named formation that declares arguments and stands under named
  formations only, since that is the unit of lowering. An anonymous
  formation, a thunk without arguments and a formation that is an atom
  already get none.
  One line comes out per formation, in the order they are written: the
  locator, then whether the body reaches for its receiver, then the name
  of every argument. What the formation answers and what each argument
  carries are not here, because only the inference knows them, and it
  does not read XMIR.
  -->
  <xsl:output encoding="UTF-8" method="text"/>
  <!--
  Whether the traversal reaches a formation at all: it walks down
  through named formations only, so an object under an application, an
  anonymous object or a λ is none of our business.
  -->
  <xsl:function name="eo:reached" as="xs:boolean">
    <xsl:param name="o" as="element(o)"/>
    <xsl:sequence select="empty($o/ancestor::o[@base or empty(@name) or @name = 'λ'])"/>
  </xsl:function>
  <xsl:template match="/">
    <xsl:for-each select="object//o[eo:reached(.)][empty(@base)][@name][@name != 'λ'][o[@base = '∅'][@name != 'ρ']][empty(o[@name = 'λ'])]">
      <xsl:value-of select="concat(@loc, '&#9;', count(descendant::o[@base = '∅' and @name = 'ρ' or starts-with(@base, 'ξ.ρ')]), '&#9;', string-join(o[@base = '∅'][@name != 'ρ']/@name, ' '), '&#10;')"/>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
