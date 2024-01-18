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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="to-xembly" version="2.0">
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:param name="testing"/>
  <xsl:variable name="EOL">
    <xsl:value-of select="'&#10;'"/>
  </xsl:variable>
  <xsl:variable name="TAB">
    <xsl:value-of select="$EOL"/>
    <xsl:value-of select="'  '"/>
  </xsl:variable>
  <xsl:template match="/sodg">
    <xsl:element name="xembly">
      <xsl:text>ADD "graph";</xsl:text>
      <xsl:value-of select="$EOL"/>
      <xsl:apply-templates select="i"/>
    </xsl:element>
  </xsl:template>
  <!-- ADD(V1) -->
  <xsl:template match="i[@name='ADD']">
    <xsl:if test="$testing = 'yes'">
      <!-- Validate the absence of vertex V1: -->
      <xsl:text>XPATH "/graph/v[@id='</xsl:text>
      <xsl:value-of select="a[1]"/>
      <xsl:text>']"; STRICT "0"; </xsl:text>
    </xsl:if>
    <!-- Add vertex V1: -->
    <xsl:text>XPATH "/graph"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ADD "v"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ATTR "id", "</xsl:text>
    <xsl:value-of select="a[1]"/>
    <xsl:text>";</xsl:text>
    <xsl:value-of select="$EOL"/>
  </xsl:template>
  <!-- BIND(V1, V2, A) -->
  <xsl:template match="i[@name='BIND']">
    <xsl:if test="$testing = 'yes'">
      <!-- Validate the presence of vertex V2: -->
      <xsl:text>XPATH "/graph/v[@id='</xsl:text>
      <xsl:value-of select="a[2]"/>
      <xsl:text>']"; STRICT "1"; </xsl:text>
      <!-- Validate the absence of V1.A edge: -->
      <xsl:text>XPATH "/graph/v[@id='</xsl:text>
      <xsl:value-of select="a[1]"/>
      <xsl:text>']/e[@title='</xsl:text>
      <xsl:value-of select="a[3]"/>
      <xsl:text>']"; STRICT "0"; </xsl:text>
    </xsl:if>
    <!-- Delete A-edge at V1 if it already exists: -->
    <xsl:text>XPATH "/graph/v[@id='</xsl:text>
    <xsl:value-of select="a[1]"/>
    <xsl:text>']/e[@title='</xsl:text>
    <xsl:value-of select="a[3]"/>
    <xsl:text>']"; REMOVE; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <!-- Go to V1: -->
    <xsl:text>XPATH "/graph/v[@id='</xsl:text>
    <xsl:value-of select="a[1]"/>
    <xsl:text>']"; STRICT "1"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <!-- Add edge: -->
    <xsl:text>ADD "e";</xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ATTR "to", "</xsl:text>
    <xsl:value-of select="a[2]"/>
    <xsl:text>"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>ATTR "title", "</xsl:text>
    <xsl:value-of select="a[3]"/>
    <xsl:text>";</xsl:text>
    <xsl:value-of select="$EOL"/>
  </xsl:template>
  <!-- PUT(V1, BYTES) -->
  <xsl:template match="i[@name='PUT']">
    <!-- Go to vertex V1: -->
    <xsl:text>XPATH "/graph/v[@id='</xsl:text>
    <xsl:value-of select="a[1]"/>
    <xsl:text>']"; STRICT "1"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <!-- Set Bytes to V1: -->
    <xsl:text>ADD "data"; </xsl:text>
    <xsl:value-of select="$TAB"/>
    <xsl:text>SET "</xsl:text>
    <xsl:value-of select="a[2]"/>
    <xsl:text>";</xsl:text>
    <xsl:value-of select="$EOL"/>
  </xsl:template>
  <xsl:template match="i[@name='COMMENT']">
    <!-- Ignore it -->
  </xsl:template>
  <xsl:template match="i">
    <xsl:message terminate="yes">
      <xsl:text>Unknown SODG '</xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:text>'</xsl:text>
    </xsl:message>
  </xsl:template>
</xsl:stylesheet>
