<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="unhome-package" version="2.0">
  <!--
  Takes the program's own package back off the references that
  "add-default-package" homed into it.

  The "parse" goal tells that sheet the qualified names of the local
  package objects, so a bare reference to a same-package object comes out
  homed: `array` in a program declaring `+package bytes` becomes
  `Φ.bytes.array`. The "format" goal prints a tree back into EO and must
  not put a prefix into a source that never carried one, so it reads the
  tree with no such awareness, where that reference stays `Φ.array`. This
  sheet stands between the two, so that "format" can read what "parse"
  already wrote instead of walking every source again.

  Only "base" and "atom" are touched, since they are all that
  "add-default-package" homes through its "eo:homed" function; "args",
  "type" and the "also" meta always go to the root "Φ" and so read back
  the same either way.

  A reference written out in full ("bytes.array" inside package "bytes")
  lands on the very same "Φ.bytes.array" as the bare one, so both come
  back bare. The prefix is only ever added when the qualified name
  belongs to the local package, so the two name one object and this is a
  matter of spelling rather than of meaning.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- The package of the current program, empty when there is no "+package" meta. -->
  <xsl:variable name="package" select="string((/object/metas/meta[head='package']/part[1])[1])"/>
  <!-- The prefix that homing into that package leaves behind. -->
  <xsl:variable name="prefix" select="concat('Φ.', $package, '.')"/>
  <xsl:template match="o/@base[$package != '' and starts-with(., $prefix)]">
    <xsl:attribute name="base" select="concat('Φ.', substring-after(., $prefix))"/>
  </xsl:template>
  <xsl:template match="o/@atom[$package != '' and starts-with(., $prefix)]">
    <xsl:attribute name="atom" select="concat('Φ.', substring-after(., $prefix))"/>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
