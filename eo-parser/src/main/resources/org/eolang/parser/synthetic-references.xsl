<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2023 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="synthetic-references" version="2.0">
  <!--
  Process alias attribute.
  @todo #2110:90m Rename synthetic-attributes to synthetic-scopes.
    We call all attributes in the transformation as "aliases".
    (you can read more about the original issue and why it is named so right
    <a href="https://github.com/objectionary/eo/issues/415">here</a>.)
    which is quite confusing and conflicts with the concept of aliases as
    foreign references. We should rename the transformation to "scopes" or
    "synthetic-scopes" and rename the "aliases" attributes in that stylesheet
    accordingly.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[@alias and count(child::o) &gt; 1]">
    <xsl:element name="o">
      <xsl:variable name="curr" select="@alias"/>
      <xsl:attribute name="abstract"/>
      <xsl:attribute name="name">
        <xsl:value-of select="concat('generated-',$curr)"/>
      </xsl:attribute>
      <xsl:attribute name="line">
        <xsl:value-of select="concat(@line, '.1')"/>
      </xsl:attribute>
      <xsl:attribute name="pos">
        <xsl:value-of select="@pos"/>
      </xsl:attribute>
      <xsl:copy>
        <xsl:copy-of select="@*"/>
        <xsl:attribute name="name">
          <xsl:text>org.eolang.</xsl:text>
          <xsl:value-of select="@alias"/>
        </xsl:attribute>
        <xsl:apply-templates select="child::o[contains(@alias, $curr)]"/>
      </xsl:copy>
      <xsl:element name="o">
        <xsl:attribute name="base">
          <xsl:text>org.eolang.</xsl:text>
          <xsl:value-of select="@alias"/>
        </xsl:attribute>
        <xsl:attribute name="name">
          <xsl:text>@</xsl:text>
        </xsl:attribute>
        <xsl:attribute name="line">
          <xsl:value-of select="concat(@line, '.2')"/>
        </xsl:attribute>
        <xsl:attribute name="pos">
          <xsl:value-of select="@pos"/>
        </xsl:attribute>
        <xsl:apply-templates select="child::o[not(@alias) or not(contains(@alias, $curr))]"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
