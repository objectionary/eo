<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="https://www.eolang.org" exclude-result-prefixes="xs eo" id="resolve-local-names" version="2.0">
  <!--
  The "&gt;&gt; foo" file-local handle (§3.10 / §9.2): the parser emits the
  anonymous object with its cactus @name plus a "@local='foo'" marker. A
  handle is an attribute of the formation that declares it, so we rewrite a
  @base equal to a handle name into that (reserved) cactus name; a handle
  declared more than once is an error. The "@local" marker is deliberately
  kept on the declaring object so that later passes (in particular the
  printer, see #5563) can recover the readable handle from the otherwise-
  synthetic cactus name instead of printing a placeholder like "vL_P".

  The rewrite is guarded by lexical scope (#5780). A reference binds to a
  handle only when the formation that declares the handle is the nearest
  enclosing scope of the reference that declares the name at all - so a
  nearer "&gt; foo" attribute (or "&gt;&gt; foo" handle) of the same name
  shadows a handle in a more distant scope, and a file-local handle no longer
  hijacks a same-named public attribute in an unrelated sibling formation.
  This mirrors how "build-fqns" later resolves a name to the nearest
  enclosing formation that owns an attribute of that name.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  The file-local handle that captures the given reference, or the empty
  sequence when the reference resolves to something else (a public attribute
  or a global). The nearest enclosing formation (an "o" without "@base") that
  declares the referenced name - as a public attribute "@name" or as a handle
  "@local" - is the reference's scope; the reference binds to a handle only
  when that scope's declaration is itself a handle.
  -->
  <xsl:function name="eo:captor" as="element()?">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="name" as="xs:string" select="$ref/@base"/>
    <xsl:variable name="scope" as="element()?" select="($ref/ancestor::o[not(@base)][o[@name=$name or @local=$name]])[last()]"/>
    <xsl:sequence select="$scope/o[@local=$name][1]"/>
  </xsl:function>
  <xsl:template match="o[@base and exists(eo:captor(.))]">
    <xsl:copy>
      <xsl:attribute name="base" select="eo:captor(.)/@name"/>
      <xsl:apply-templates select="@* except @base"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="/object">
    <xsl:copy>
      <xsl:apply-templates select="(node() except errors)|@*"/>
      <xsl:variable name="errors" as="element()*">
        <xsl:for-each-group select="//o[@local]" group-by="@local">
          <xsl:if test="count(current-group()) &gt; 1">
            <xsl:element name="error">
              <xsl:attribute name="check" select="'resolve-local-names'"/>
              <xsl:attribute name="line" select="if (current-group()[2]/@line) then current-group()[2]/@line else 0"/>
              <xsl:attribute name="severity" select="'error'"/>
              <xsl:text>duplicate local name '</xsl:text>
              <xsl:value-of select="current-grouping-key()"/>
              <xsl:text>'</xsl:text>
            </xsl:element>
          </xsl:if>
        </xsl:for-each-group>
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
