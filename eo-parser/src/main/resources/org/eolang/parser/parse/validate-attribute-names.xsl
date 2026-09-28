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
  <!--
  Every declared attribute, by its owner and its name. The check used to
  ask each attribute whether a preceding sibling carried the same name,
  which is one sibling scan per attribute and N squared comparisons for a
  formation of N attributes (#8532). The index answers the same question
  by position: a name declared twice has a second member in its group,
  and keys answer in document order, so "not the first one" is exactly
  "has a preceding sibling of the same name".
  -->
  <xsl:key name="named" match="o[@name][not(@base='∅')]" use="concat(generate-id(..), ' ', @name)"/>
  <xsl:template match="/object">
    <xsl:variable name="errors" as="element()*">
      <xsl:for-each select="descendant-or-self::*[self::object or self::o[not(@base)]]/o[@name and not(@base='∅')]">
        <xsl:variable name="name" select="@name"/>
        <xsl:if test="not(. is key('named', concat(generate-id(..), ' ', $name), root(.))[1])">
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
