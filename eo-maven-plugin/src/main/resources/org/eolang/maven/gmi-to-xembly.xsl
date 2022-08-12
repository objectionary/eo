<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2022 Objectionary.com

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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="gmi-to-xembly" version="2.0">
  <xsl:output encoding="UTF-8" method="text"/>
  <xsl:variable name="EOL">
    <xsl:value-of select="'&#10;'"/>
  </xsl:variable>
  <xsl:variable name="TAB">
    <xsl:value-of select="$EOL"/>
    <xsl:value-of select="'  '"/>
  </xsl:variable>
  <xsl:template match="/">
    <xsl:text>ADD "graph"; </xsl:text>
    <xsl:apply-templates select="program/gmi/i"/>
  </xsl:template>
  <xsl:template match="i[@name='ADD']">
    <xsl:text>XPATH "/graph"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ADD "v"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ATTR "id", "</xsl:text>
    <xsl:value-of select="a[1]"/>
    <xsl:text>";</xsl:text>
    <xsl:value-of select="$EOL"/>
  </xsl:template>
  <xsl:template match="i[@name='BIND']">
    <xsl:text>XPATH "/graph/v[@id='</xsl:text>
    <xsl:value-of select="a[2]"/>
    <xsl:text>']"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ADD "e";</xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ATTR "id", "</xsl:text>
    <xsl:value-of select="a[1]"/>
    <xsl:text>"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ATTR "to", "</xsl:text>
    <xsl:value-of select="a[3]"/>
    <xsl:text>"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ATTR "title", "</xsl:text>
    <xsl:value-of select="a[4]"/>
    <xsl:text>";</xsl:text>
    <xsl:if test="a[4] != '^'">
      <xsl:value-of select="$TAB"/>
      <xsl:text>XPATH "/graph/v[@id='</xsl:text>
      <xsl:value-of select="a[3]"/>
      <xsl:text>']"; </xsl:text>
      <xsl:value-of select="$TAB"/>
      <xsl:text>ADD "e"; </xsl:text>
      <xsl:value-of select="$TAB"/>
      <xsl:text>ATTR "id", "</xsl:text>
      <xsl:value-of select="a[1]"/>
      <xsl:text>.up"; </xsl:text>
      <xsl:value-of select="$TAB"/>
      <xsl:text>ATTR "to", "</xsl:text>
      <xsl:value-of select="a[2]"/>
      <xsl:text>"; </xsl:text>
      <xsl:value-of select="$TAB"/>
      <xsl:text>ATTR "title", "^";</xsl:text>
    </xsl:if>
    <xsl:value-of select="$EOL"/>
  </xsl:template>
  <xsl:template match="i[@name='REF']">
    <xsl:text>XPATH "/graph/v[@id='</xsl:text>
    <xsl:value-of select="a[2]"/>
    <xsl:text>']"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ADD "e"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ATTR "id", "</xsl:text>
    <xsl:value-of select="a[1]"/>
    <xsl:text>"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ATTR "to", "</xsl:text>
    <xsl:value-of select="a[3]"/>
    <xsl:text>"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ATTR "title", "</xsl:text>
    <xsl:value-of select="a[4]"/>
    <xsl:text>";</xsl:text>
    <xsl:value-of select="$EOL"/>
  </xsl:template>
  <xsl:template match="i[@name='DATA']">
    <xsl:text>XPATH "/graph/v[@id='</xsl:text>
    <xsl:value-of select="a[1]"/>
    <xsl:text>']"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ADD "data"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>SET "</xsl:text>
    <xsl:value-of select="a[2]"/>
    <xsl:text>";</xsl:text>
    <xsl:value-of select="$EOL"/>
  </xsl:template>
</xsl:stylesheet>
