<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="validate-attribute-names" version="2.0">
  <!--
  Here we add an error with severity 'critical' when one formation declares
  two attributes with the same name, since the second one would silently
  replace the first one in the object built at run time. Voids are left out,
  they are a different declaration and are checked elsewhere.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object">
    <xsl:variable name="errors" as="element()*">
      <xsl:for-each select="descendant-or-self::*[self::object or self::o[not(@base)]]/o[@name and not(@base='∅')]">
        <xsl:variable name="name" select="@name"/>
        <xsl:if test="preceding-sibling::o[@name=$name and not(@base='∅')]">
          <error>
            <xsl:attribute name="check" select="'validate-attribute-names'"/>
            <xsl:attribute name="line" select="if (@line) then @line else 0"/>
            <xsl:attribute name="severity" select="'critical'"/>
            <xsl:if test="@pos">
              <xsl:attribute name="pos" select="@pos"/>
            </xsl:if>
            <xsl:value-of select="concat('Attribute &quot;', $name, '&quot; is declared more than once in the same formation')"/>
          </error>
        </xsl:if>
      </xsl:for-each>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="(node() except errors)|@*"/>
      <xsl:if test="exists($errors) or exists(/object/errors)">
        <errors>
          <xsl:apply-templates select="/object/errors/error"/>
          <xsl:copy-of select="$errors"/>
        </errors>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
