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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" id="build-fqns" version="2.0">
  <!--
  Here we go through all objects and find what their @base
  are referring to. If we find the object they refer to,
  we add either $ object (if target object in the same scope)
  or necessary amount of ^. objects. Those objects
  which are skipped after this transformation
  are not visible in the current scope. Maybe they are
  global or just a mistake.

  We must skip objects that refer to
  "bytes", "string" or "number" if such objects are inside the
  "org.eolang.bytes", "org.eolang.string" or "org.eolang.bytes".
  Such a reference would be misleading: instead of referring to the
  global, for example, "org.eolang.bytes" they will lead to local "bytes"
  defined in this particular file.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:variable name="primitives" as="element()*">
    <a>bytes</a>
    <a>string</a>
    <a>number</a>
  </xsl:variable>
  <xsl:variable name="this">
    <xsl:element name="o">
      <xsl:attribute name="base" select="'$'"/>
    </xsl:element>
  </xsl:variable>
  <!-- Convert given object to method -->
  <xsl:template match="o" mode="to-method">
    <xsl:param name="of"/>
    <xsl:element name="o">
      <xsl:attribute name="base">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="@base"/>
      </xsl:attribute>
      <xsl:apply-templates select="@* except @base"/>
      <xsl:copy-of select="$of"/>
      <xsl:apply-templates select="o"/>
    </xsl:element>
  </xsl:template>
  <!-- ADD ^. TO GIVEN OBJECT (OR NOT) -->
  <xsl:template match="o" mode="with-rho">
    <xsl:param name="rhos"/>
    <xsl:param name="current"/>
    <xsl:choose>
      <!-- No need to add rho - we're in the current scope, but in application -->
      <xsl:when test="$rhos=0">
        <xsl:apply-templates select="." mode="to-method">
          <xsl:with-param name="of" select="$this"/>
        </xsl:apply-templates>
      </xsl:when>
      <!-- We're in the end of recursion -->
      <xsl:when test="$rhos=1">
        <xsl:apply-templates select="." mode="to-method">
          <xsl:with-param name="of" select="$current"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates mode="with-rho" select=".">
          <xsl:with-param name="rhos" select="$rhos - 1"/>
          <xsl:with-param name="current">
            <xsl:element name="o">
              <xsl:attribute name="base" select="'.^'"/>
              <xsl:copy-of select="$current"/>
            </xsl:element>
          </xsl:with-param>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- BUILD FQN PATH TO OBJECT -->
  <xsl:template match="o|objects" mode="fqn">
    <xsl:param name="rhos"/>
    <xsl:param name="self"/>
    <xsl:param name="find"/>
    <xsl:variable name="parent" select="parent::*"/>
    <xsl:choose>
      <!-- last frontier -->
      <xsl:when test="$parent[name()='objects']">
        <xsl:element name="o">
          <xsl:apply-templates select="$self/o|$self/@*"/>
        </xsl:element>
      </xsl:when>
      <xsl:when test="eo:abstract($parent)">
        <xsl:choose>
          <!-- Found reference in some abstract object above -->
          <xsl:when test="$parent/o[@name=$find]">
            <xsl:apply-templates select="$self" mode="with-rho">
              <xsl:with-param name="rhos" select="$rhos"/>
              <xsl:with-param name="current">
                <xsl:element name="o">
                  <xsl:attribute name="base" select="'^'"/>
                </xsl:element>
              </xsl:with-param>
            </xsl:apply-templates>
          </xsl:when>
          <!-- No reference - go upper -->
          <xsl:otherwise>
            <xsl:apply-templates select="$parent" mode="fqn">
              <xsl:with-param name="self" select="$self"/>
              <xsl:with-param name="rhos" select="$rhos + 1"/>
              <xsl:with-param name="find" select="$find"/>
            </xsl:apply-templates>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <!-- Parent is not abstract (application) - try to find reference upper -->
      <xsl:otherwise>
        <xsl:apply-templates select="$parent" mode="fqn">
          <xsl:with-param name="self" select="$self"/>
          <xsl:with-param name="find" select="$find"/>
          <xsl:with-param name="rhos" select="$rhos"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- ENTRY POINT -->
  <xsl:template match="o[not($primitives/text()=@base and /program/objects/o/@name=@base and /program/metas/meta[head='package' and tail='org.eolang'])]">
    <xsl:apply-templates select="." mode="not-primitive"/>
  </xsl:template>
  <xsl:template match="o[@base]" mode="not-primitive">
    <xsl:apply-templates select="." mode="with-base"/>
  </xsl:template>
  <xsl:template match="o[not(contains(@base, '.'))]" mode="with-base">
    <xsl:apply-templates select="." mode="no-dots"/>
  </xsl:template>
  <xsl:template match="o[@base!='$' and @base!='^' and @base!='∅']" mode="no-dots">
    <xsl:variable name="base"  select="./@base"/>
    <xsl:choose>
      <!-- Closes object in the same scope -->
      <xsl:when test="parent::o/o[@name=$base]">
        <xsl:apply-templates select="." mode="to-method">
          <xsl:with-param name="of" select="$this"/>
        </xsl:apply-templates>
      </xsl:when>
      <!-- Closest object in the same scope, but global -->
      <xsl:when test="parent::objects/o[@name=$base]">
        <xsl:copy-of select="."/>
      </xsl:when>
      <!--- Try to find the closest object in parents -->
      <xsl:otherwise>
        <xsl:apply-templates select="." mode="fqn">
          <xsl:with-param name="self" select="."/>
          <xsl:with-param name="find" select="$base"/>
          <xsl:with-param name="rhos" select="0"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
