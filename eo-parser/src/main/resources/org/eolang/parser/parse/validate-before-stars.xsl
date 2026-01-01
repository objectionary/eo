<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs" id="validate-before-stars" version="2.0">
  <!--
    Checks if index after '*' in compact array syntax is more than amount of arguments.

    Correct:
    ```
    sprintf *1
      "Hello, %s"
      "world"
    ```

    Incorrect:
    ```
    sprintf *3
      "Hello, %s"
      "world"
    ```
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="/object">
    <xsl:copy>
      <xsl:apply-templates select="(node() except errors)|@*"/>
      <xsl:variable name="errors" as="element()*">
        <xsl:for-each select="//o[@before-star &gt; count(o)]">
          <xsl:element name="error">
            <xsl:attribute name="check" select="'validate-before-stars'"/>
            <xsl:attribute name="line" select="if (@line) then @line else 0"/>
            <xsl:attribute name="severity" select="'error'"/>
            <xsl:text>Index after '*' (</xsl:text>
            <xsl:value-of select="@before-star"/>
            <xsl:text>) must be less than amount arguments (</xsl:text>
            <xsl:value-of select="count(o)"/>
            <xsl:text>)</xsl:text>
          </xsl:element>
        </xsl:for-each>
      </xsl:variable>
      <xsl:if test="not(empty($errors)) or exists(/object/errors)">
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
