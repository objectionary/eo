<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2024 Objectionary.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
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
  <xsl:template match="/program">
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
      <xsl:if test="not(empty($errors)) or exists(/program/errors)">
        <errors>
          <xsl:apply-templates select="/program/errors/error"/>
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
