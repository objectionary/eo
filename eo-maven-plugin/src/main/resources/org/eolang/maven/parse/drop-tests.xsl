<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="drop-tests" version="3.0">
  <!--
  Drop every unit test from the XMIR. The parser names a test attribute
  "p🌵name" when it must be true, and "n🌵name" when it must throw.
  Every "o" with such a name goes away, together with its body,
  wherever it is.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[starts-with(@name, 'p🌵') or starts-with(@name, 'n🌵')]"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
