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
  points at that cactus name is rewritten to the handle, so references read
  under the handle instead of a synthetic "vL_P" placeholder. The
  "@local" marker is deliberately KEPT on the void (#5581) so that
  "to-eo-tree" can print it back as a vertical "? &gt;&gt; name" line and
  preserve the void's anonymity (§9.2, R-9.2.3), rather than collapsing it
  into a public "[name]" bracket param. A non-void "&gt;&gt;" handle (on an
  anonymous formation) is left to the existing "inline-cactoos" pass, so its
  @local marker is simply dropped.
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
  <!-- Keep the marker on voids so "to-eo-tree" restores the readable
       "? &gt;&gt; name" handle; drop it on non-void formations, whose
       handle is inlined away by the "inline-cactoos" pass. -->
  <xsl:template match="o[not(@base=$eo:empty)]/@local"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
