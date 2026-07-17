<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="atoms-table" version="2.0">
  <!--
  For each lambda atom (`<o name="λ" atom="...">`) emit an `<atom>` element
  whose `forma` attribute is the dotted name of the atom's enclosing
  formation prefixed with the package (if declared) and a leading `Φ.`,
  and whose `returns` attribute is the value of the `atom` attribute on
  the lambda. Lambdas without a declared return type (empty `atom`) are
  skipped. The collected list is wrapped in a single `<atoms>` element.
  -->
  <xsl:output encoding="UTF-8" indent="yes" method="xml"/>
  <xsl:variable name="pkg" select="/object/metas/meta[head='package']/part[1]/text()"/>
  <xsl:template match="/">
    <atoms>
      <xsl:for-each select="//o[@name='λ' and @atom and string-length(@atom) &gt; 0]">
        <xsl:variable name="chain">
          <xsl:for-each select="ancestor::o[@name and @name!='λ']">
            <xsl:if test="position() &gt; 1">
              <xsl:text>.</xsl:text>
            </xsl:if>
            <xsl:value-of select="@name"/>
          </xsl:for-each>
        </xsl:variable>
        <atom>
          <xsl:attribute name="forma">
            <xsl:text>Φ.</xsl:text>
            <xsl:if test="$pkg and string-length($pkg) &gt; 0">
              <xsl:value-of select="$pkg"/>
              <xsl:text>.</xsl:text>
            </xsl:if>
            <xsl:value-of select="$chain"/>
          </xsl:attribute>
          <xsl:attribute name="returns">
            <xsl:value-of select="@atom"/>
          </xsl:attribute>
        </atom>
      </xsl:for-each>
    </atoms>
  </xsl:template>
</xsl:stylesheet>
