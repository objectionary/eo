<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="restore-local-names" version="2.0">
  <!--
  Inverse of the parser's "resolve-local-names" pass, applied before
  printing (#5563). A void declared with a file-local handle
  ("? &gt;&gt; name", R-3.10.12) keeps a synthetic cactus @name plus a
  "@local='name'" marker; the parser resolves references to the cactus name
  but the readable handle is preserved on the void. Here we put the handle
  back: the void's @name becomes its handle and every @base segment that
  points at that cactus name is rewritten to the handle, so the printer
  emits "[name]" and "name" instead of a synthetic "vL_P" placeholder. A
  non-void "&gt;&gt;" handle (on an anonymous formation) is left to the
  existing "inline-cactoos" pass, so its @local marker is simply dropped.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:key name="void-handle" match="o[@local and @base=$eo:empty]" use="@name"/>
  <!-- References: rewrite each cactus segment that names a handled void
       back into the readable handle. -->
  <xsl:template match="@base">
    <xsl:attribute name="base" select="string-join(for $seg in tokenize(., '\.') return (if (key('void-handle', $seg)) then key('void-handle', $seg)[1]/@local else $seg), '.')"/>
  </xsl:template>
  <!-- Void declaration: promote the handle to the visible name. -->
  <xsl:template match="o[@local and @base=$eo:empty]/@name">
    <xsl:attribute name="name" select="../@local"/>
  </xsl:template>
  <!-- Drop the marker (already applied on voids above; unused on formations). -->
  <xsl:template match="o/@local"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
