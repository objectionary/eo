<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs" id="restore-local-names" version="2.0">
  <!--
  Inverse of the parser's "resolve-local-names" pass, applied before
  printing (#5563). An object declared with a file-local handle ("&gt;&gt;
  name", §3.10 / §9.2) keeps a synthetic cactus @name plus a "@local='name'"
  marker; the parser resolves references to the cactus name but the readable
  handle is preserved on the declaration. Here we put the handle back: the
  declaration's @name becomes its handle and every @base segment that points
  at that cactus name is rewritten to the handle, so references read under the
  handle instead of a synthetic "vL_P" placeholder. The "@local" marker is
  deliberately KEPT (#5581) so that "to-eo-tree" prints it back as a
  "&gt;&gt; name" line (a vertical "? &gt;&gt; name" for a void, R-9.2.3),
  rather than collapsing it into a public "[name]" bracket param.

  Every handle is restored, not just voids and recursive formations: a handle
  is a name the author chose, so it must survive to the printed source
  regardless of whether "inline-cactoos" would otherwise inline the object
  away. Promoting the cactus @name to the handle here (before "inline-cactoos"
  runs) leaves no cactus name for that pass to match, so a handled object is
  never inlined; a truly anonymous auto-named object (no "@local") still is.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:key name="handle" match="o[@local]" use="@name"/>
  <!--
  References: rewrite each cactus segment that names a handled object back
  into its readable handle.
  -->
  <xsl:template match="@base">
    <xsl:attribute name="base" select="string-join(for $seg in tokenize(., '\.') return (if (key('handle', $seg)) then key('handle', $seg)[1]/@local else $seg), '.')"/>
  </xsl:template>
  <!--
  Handled declaration: promote the handle to the visible name.
  -->
  <xsl:template match="o[@local]/@name">
    <xsl:attribute name="name" select="../@local"/>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
