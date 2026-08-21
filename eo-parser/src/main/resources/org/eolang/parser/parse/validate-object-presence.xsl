<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="validate-object-presence" version="2.0">
  <!--
  Here we add error with severity 'critical' if the file declares no object.

  A file that holds nothing but metas or a top comment block used to parse
  without a single error, since 'validate-objects-count' deliberately says
  nothing about the absence of an object (#4139) and only rejects a second
  one. Such a file compiles to nothing and cannot even be printed back, so
  it is refused here instead (#6713).

  Incorrect:
  ```
  +package foo
  ```

  The check keeps quiet when the file already carries an error, because an
  object missing after broken syntax is a consequence of that syntax and a
  second message about it only misleads.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object">
    <xsl:variable name="error" as="element()*">
      <xsl:if test="empty(o) and empty(errors/error)">
        <error>
          <xsl:attribute name="check" select="'validate-object-presence'"/>
          <xsl:attribute name="line" select="0"/>
          <xsl:attribute name="severity" select="'critical'"/>
          <xsl:text>Every source file must declare an object, this one declares none</xsl:text>
        </error>
      </xsl:if>
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
