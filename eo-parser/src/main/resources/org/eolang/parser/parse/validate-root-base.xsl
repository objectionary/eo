<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="validate-root-base" version="3.0">
  <!--
  Here we add an error with severity 'critical' when the object of the file
  is based on 'ξ', which is what '$' and '^' turn into.

  A file holds one object and that object is the program. There is no
  formation around it, so '$' points at nothing and '^' has no parent to
  reach. The transpiler used to accept such a program and write Java that
  does not compile, like 'Phi r = ;' (#9345).

  Incorrect:
  ```
  $.foo > app
  ```
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object">
    <xsl:variable name="error" as="element()*">
      <xsl:for-each select="o[@base = 'ξ' or starts-with(@base, 'ξ.')]">
        <error>
          <xsl:attribute name="check" select="'validate-root-base'"/>
          <xsl:attribute name="line" select="if (@line) then @line else 0"/>
          <xsl:attribute name="severity" select="'critical'"/>
          <xsl:text>The object of a source file cannot be based on '$' or '^', since nothing surrounds it</xsl:text>
        </error>
      </xsl:for-each>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="(node() except errors)|@*"/>
      <xsl:if test="exists($error) or exists(/object/errors)">
        <errors>
          <xsl:apply-templates select="/object/errors/error"/>
          <xsl:copy-of select="$error"/>
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
