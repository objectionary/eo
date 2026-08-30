<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="build-fqns" version="2.0">
  <!-- An FQN-resolution stage; its per-mode templates form one cohesive module. -->
  <!-- xslint-disable-file too-many-templates -->
  <!--
  Here we go through all objects and find what their @base
  are referring to. If we find the object they refer to in the
  current scope, we add the $ object in front of it. A name that
  lives in an enclosing scope instead is reported: this stage used
  to walk up and insert the '^.' hops itself, which made the same
  source text mean one thing here and another one level down, so
  the hops are the author's to write now. A cactus name - what a
  '&gt;&gt;' handle resolves to - never reaches this rule at all: it
  arrives from "resolve-local-names.xsl" with its receiver, ξ or the
  right number of '.ρ' hops, already built, the same way an explicit
  "^.foo" a real author wrote does, so no name is resolved across a
  scope the author did not write. Those objects which are skipped
  after this transformation are not visible in the current scope.
  Maybe they are global or just a mistake.

  Both errors this stage reports are marked "lossy": the reference
  that caused them survives into the tree, but with the meaning the
  author wrote stripped off it - the '@hop' marker is gone and
  "add-default-package.xsl" homes the still dot-less name into the
  root package, so printing the tree back gives "Q.x" where the
  source said the parent's "x". A stage that reads the printed form
  as canonical - "eo:format", above all - therefore has to refuse
  the file instead of writing that text over it (#7862).

  We must skip objects that refer to
  "bytes", "string" or "number" if such objects are inside the
  "Q.bytes", "Q.string" or "Q.bytes".
  Such a reference would be misleading: instead of referring to the
  global, for example, "Q.bytes" they will lead to local "bytes"
  defined in this particular file.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:variable name="this">
    <o>
      <xsl:attribute name="base" select="'ξ'"/>
    </o>
  </xsl:variable>
  <!--
  Every named attribute, indexed by its name together with the object that
  owns it. The scope walk below asks "does this object declare this name" at
  every enclosing object of every reference, and answered it by scanning that
  object's children - twice, the question being put twice in one
  "xsl:choose". Each answer is a hash lookup now, the way
  "resolve-local-names.xsl" indexes the same question (#6502, #7938). The
  "+package" below is likewise read once, not per reference.
  -->
  <xsl:key name="attributes" match="o[@name]" use="concat(@name, '#', generate-id(..))"/>
  <xsl:variable name="eo:package" select="string((/object/metas/meta[head='package'])[1]/part[1])"/>
  <!-- Build recursive objects chain from package if exists -->
  <xsl:template match="o" mode="recursive-package">
    <xsl:param name="pkg"/>
    <xsl:choose>
      <xsl:when test="$pkg='' or empty($pkg)">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="last">
          <xsl:choose>
            <xsl:when test="contains($pkg, '.')">
              <xsl:value-of select="substring-before($pkg, '.')"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$pkg"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:variable name="elem">
          <o>
            <xsl:attribute name="base">
              <xsl:text>.</xsl:text>
              <xsl:value-of select="$last"/>
            </xsl:attribute>
            <xsl:copy-of select="."/>
          </o>
        </xsl:variable>
        <xsl:apply-templates select="$elem" mode="recursive-package">
          <xsl:with-param name="pkg" select="substring-after($pkg, '.')"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- Copy given element and append package if exists -->
  <xsl:template match="o" mode="with-package">
    <xsl:param name="parent"/>
    <xsl:param name="find"/>
    <xsl:choose>
      <xsl:when test="exists(key('attributes', concat($find, '#', generate-id($parent))))">
        <xsl:variable name="start">
          <o>
            <xsl:attribute name="base" select="'Φ'"/>
          </o>
        </xsl:variable>
        <xsl:apply-templates select="." mode="to-method">
          <xsl:with-param name="of">
            <xsl:apply-templates select="$start" mode="recursive-package">
              <xsl:with-param name="pkg" select="$eo:package"/>
            </xsl:apply-templates>
          </xsl:with-param>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="node()|@*"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- Convert given object to method -->
  <xsl:template match="o" mode="to-method">
    <xsl:param name="of"/>
    <o>
      <xsl:attribute name="base">
        <xsl:text>.</xsl:text>
        <xsl:value-of select="@base"/>
      </xsl:attribute>
      <xsl:apply-templates select="@* except @base"/>
      <xsl:copy-of select="$of"/>
      <xsl:apply-templates select="o"/>
    </o>
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
      <!-- Next iteration -->
      <xsl:otherwise>
        <xsl:apply-templates mode="with-rho" select=".">
          <xsl:with-param name="rhos" select="$rhos - 1"/>
          <xsl:with-param name="current">
            <o>
              <xsl:attribute name="base" select="'.ρ'"/>
              <xsl:copy-of select="$current"/>
            </o>
          </xsl:with-param>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- BUILD FQN PATH TO OBJECT -->
  <xsl:template match="o" mode="fqn">
    <xsl:param name="rhos"/>
    <xsl:param name="self"/>
    <xsl:param name="find"/>
    <xsl:variable name="parent" select="parent::*"/>
    <!-- Whether this enclosing object declares the name being resolved. -->
    <xsl:variable name="declares" as="xs:boolean" select="exists(key('attributes', concat($find, '#', generate-id($parent))))"/>
    <xsl:choose>
      <!-- last frontier -->
      <xsl:when test="$parent[name()='object']">
        <xsl:apply-templates select="$self" mode="with-package">
          <xsl:with-param name="find" select="$find"/>
          <xsl:with-param name="parent" select="$parent"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:when test="eo:abstract($parent)">
        <xsl:choose>
          <!-- Found reference in the current scope -->
          <xsl:when test="$declares and $rhos=0">
            <xsl:apply-templates select="$self" mode="with-rho">
              <xsl:with-param name="rhos" select="$rhos"/>
              <xsl:with-param name="current">
                <o>
                  <xsl:attribute name="base" select="'.ρ'"/>
                  <xsl:copy-of select="$this"/>
                </o>
              </xsl:with-param>
            </xsl:apply-templates>
          </xsl:when>
          <!-- Found reference in some abstract object above -->
          <xsl:when test="$declares">
            <o>
              <xsl:apply-templates select="$self/@*"/>
              <xsl:attribute name="hop" select="$rhos"/>
              <xsl:apply-templates select="$self/node()"/>
            </o>
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
  <xsl:template match="o[@base]">
    <xsl:apply-templates select="." mode="with-base"/>
  </xsl:template>
  <xsl:template match="o[not(contains(@base, '.'))]" mode="with-base">
    <xsl:apply-templates select="." mode="no-dots"/>
  </xsl:template>
  <xsl:template match="o[@base='ρ']" mode="no-dots">
    <o>
      <xsl:apply-templates select="@* except @base"/>
      <xsl:attribute name="base" select="'.ρ'"/>
      <o>
        <xsl:attribute name="line" select="@line"/>
        <xsl:attribute name="pos" select="@pos - 1"/>
        <xsl:attribute name="base" select="'ξ'"/>
      </o>
      <xsl:apply-templates select="o"/>
    </o>
  </xsl:template>
  <xsl:template match="o[@base!='ξ' and @base!='ρ' and @base!=$eo:empty and @base!=$eo:bottom]" mode="no-dots">
    <xsl:variable name="base" select="./@base"/>
    <xsl:apply-templates select="." mode="fqn">
      <xsl:with-param name="self" select="."/>
      <xsl:with-param name="find" select="$base"/>
      <xsl:with-param name="rhos" select="0"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="node()|@*" mode="#all">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <!-- Drop the "@hop" marker once the report below has been collected -->
  <xsl:template match="@hop" mode="stripped" priority="2"/>
  <xsl:template match="node()|@*" mode="stripped" priority="1">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*" mode="stripped"/>
    </xsl:copy>
  </xsl:template>
  <!--
  A "φ" reference left unresolved by "with-package" above stays as a
  literal "<o base='φ'>" in the transformed tree (nothing rewrites it).
  Reporting this here, after the transform, rather than terminating the
  whole XSL train mid-resolution (as this file used to), lets it surface
  as a normal <errors> entry with the offending line, consistent with
  every other diagnostic in this pipeline (see #6042). A name found only
  in an enclosing scope is marked with "@hop" the same way and reported
  the same way, then the marker is stripped so it never reaches the
  next stage.
  -->
  <xsl:template match="/object">
    <xsl:variable name="transformed" as="item()*">
      <xsl:apply-templates select="(node() except errors)|@*"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="$transformed" mode="stripped"/>
      <xsl:variable name="errors" as="element()*">
        <xsl:for-each select="$transformed//o[@base='φ']">
          <error>
            <xsl:attribute name="check" select="'build-fqns'"/>
            <xsl:attribute name="line" select="if (@line) then @line else 0"/>
            <xsl:attribute name="severity" select="'error'"/>
            <xsl:attribute name="lossy" select="''"/>
            <xsl:text>The φ object is used, but absent in self or parents scope</xsl:text>
          </error>
        </xsl:for-each>
        <xsl:for-each select="$transformed//o[@hop]">
          <error>
            <xsl:attribute name="check" select="'build-fqns'"/>
            <xsl:attribute name="line" select="if (@line) then @line else 0"/>
            <xsl:attribute name="severity" select="'error'"/>
            <xsl:attribute name="lossy" select="''"/>
            <xsl:text>The "</xsl:text>
            <xsl:value-of select="@base"/>
            <xsl:text>" object is declared in an enclosing scope, write it as "</xsl:text>
            <xsl:value-of select="string-join(for $hop in 1 to xs:integer(@hop) return '^.', '')"/>
            <xsl:value-of select="@base"/>
            <xsl:text>"</xsl:text>
          </error>
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
</xsl:stylesheet>
