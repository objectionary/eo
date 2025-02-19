<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2025 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="_specials" version="2.0">
  <xsl:variable name="eo:alpha" select="'α'"/>
  <xsl:variable name="eo:xi" select="'ξ'"/>
  <xsl:variable name="eo:delta" select="'Δ'"/>
  <xsl:variable name="eo:phi" select="'φ'"/>
  <xsl:variable name="eo:rho" select="'ρ'"/>
  <xsl:variable name="eo:program" select="'Φ'"/>
  <xsl:variable name="eo:def-package" select="'Φ̇'"/>
  <xsl:variable name="eo:lambda" select="'λ'"/>
  <xsl:variable name="eo:arrow">
    <xsl:value-of select="$eo:space"/>
    <xsl:text>↦</xsl:text>
    <xsl:value-of select="$eo:space"/>
  </xsl:variable>
  <xsl:variable name="eo:dashed-arrow">
    <xsl:value-of select="$eo:space"/>
    <xsl:text>⤍</xsl:text>
    <xsl:value-of select="$eo:space"/>
  </xsl:variable>
  <xsl:variable name="eo:lb" select="'⟦'"/>
  <xsl:variable name="eo:rb" select="'⟧'"/>
  <xsl:variable name="eo:clb" select="'('"/>
  <xsl:variable name="eo:crb" select="')'"/>
  <xsl:variable name="eo:empty" select="'∅'"/>
  <xsl:variable name="eo:space" select="' '"/>
  <xsl:variable name="eo:new-line" select="'&#10;'"/>
</xsl:stylesheet>
