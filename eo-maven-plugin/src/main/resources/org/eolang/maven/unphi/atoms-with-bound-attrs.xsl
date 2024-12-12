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
<xsl:stylesheet xmlns:eo="https://www.eolang.org" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="atoms-with-bound-attrs" version="2.0">
  <!--
  Convert such expression in XMIR:
    x ↦ ⟦
      text ↦ Φ.org.eolang.bytes (α0 ↦ ⟦ Δ ⤍ 48-65-6C-6C-6F-20-77-6F-72-6C-64-0A ⟧),
      size ↦ Φ.org.eolang.bytes (α0 ↦ ⟦ Δ ⤍ 00-00-00-00-00-00-00-08 ⟧),
      λ ⤍ Lorg_eolang_io_stdout
    ⟧
  to this:
    x ↦ Φ.org.eolang.io.stdout(
      text ↦ Φ.org.eolang.bytes (α0 ↦ ⟦ Δ ⤍ 48-65-6C-6C-6F-20-77-6F-72-6C-64-0A ⟧),
      size ↦ Φ.org.eolang.bytes (α0 ↦ ⟦ Δ ⤍ 00-00-00-00-00-00-00-08 ⟧)
    )
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[eo:abstract(.) and @atom and starts-with(@atom, 'L') and @name and not(o[@base='∅']) and count(o[@base!='∅'])&gt;0]">
    <xsl:element name="o">
      <xsl:attribute name="name" select="@name"/>
      <xsl:attribute name="base">
        <xsl:for-each select="tokenize(@atom,'_')">
          <xsl:choose>
            <xsl:when test="position()=1">
              <xsl:value-of select="substring(., 2)"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>.</xsl:text>
              <xsl:choose>
                <xsl:when test=".='φ'">
                  <xsl:text>@</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="."/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:attribute>
      <xsl:for-each select="o">
        <xsl:element name="o">
          <xsl:for-each select="@*[name()!='name']">
            <xsl:attribute name="{name()}" select="."/>
          </xsl:for-each>
          <xsl:attribute name="as" select="@name"/>
          <xsl:copy-of select="*"/>
        </xsl:element>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
