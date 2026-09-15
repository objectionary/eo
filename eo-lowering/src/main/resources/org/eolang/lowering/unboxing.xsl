<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs" id="unboxing" version="2.0">
  <!--
  Here we take the boxes back out. A box is an "o" named "λ" holding the
  name the engine answers under, planted by "boxing.xsl" so that phino
  would fire the engine instead of entering the body as written. Whatever
  phino hands back may carry one of them along, and a box means nothing
  to anybody downstream: it is neither an atom the transpiler knows nor a
  marker the build turns into one. So every box goes, and the formation
  that held it is a formation as written again.
  Nothing else here is touched. The λ of an atom stays, the marker of a
  symbol stays, and so does every attribute of every object, because the
  document is the output of the engine and we are not the ones to edit
  it.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- The head every box name is spelled with, which the build owns -->
  <xsl:param name="prefix" as="xs:string" required="yes"/>
  <xsl:template match="o[@name='λ'][empty(@base)][empty(*)][starts-with(normalize-space(.), $prefix)]"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
