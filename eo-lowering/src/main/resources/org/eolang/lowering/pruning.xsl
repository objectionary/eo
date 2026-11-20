<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="pruning" version="3.0">
  <!--
  Here we cut the tests out of one XMIR file. A test is a binding the parser
  names with a mark, "p🌵" for one that must hold and "n🌵" for one that
  must fail, and everything under such a binding is the program of that
  test and of nothing else, so the binding goes with all it holds, wherever
  in the file it stands. Every other node is copied as it is, attributes and
  order and all: what is left is the object exactly as its author wrote it,
  with nothing said about it.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="o[starts-with(@name, 'p🌵') or starts-with(@name, 'n🌵')]"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
