<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" exclude-result-prefixes="eo" id="void-reads" version="3.0">
  <!--
  Here we list every read of a hidden void (#9788) in an object that has
  just taken the members of its package in. A void can be read only in the
  file that declares it, and the parser hides every such read, together
  with the void, behind "v🌵". A member is a file of its own, so a read it
  makes through its receiver climbs out of its file and the parser leaves
  it as written. Inside the object, the read lands in a formation that has
  the void "v🌵x" and no attribute "x". This step finds that read.

  Each read is listed with the member it comes from, the void it reads and
  the object that declares the void, all three as their locators without
  the leading "Φ.".
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" indent="yes" method="xml"/>
  <xsl:template match="/">
    <reads>
      <xsl:for-each select="/object//o[starts-with(@base, concat($eo:xi, '.')) or starts-with(@base, '.')]">
        <xsl:variable name="formation" select="eo:own-formation(.)"/>
        <xsl:variable name="name" select="eo:own-name(.)"/>
        <xsl:if test="exists($formation/o[eo:void(.) and @name=concat($eo:veiled, $name)]) and empty($formation/o[@name=$name])">
          <read member="{substring-after(ancestor-or-self::o[parent::o is $formation]/@loc, concat($eo:program, '.'))}" void="{$name}" object="{substring-after($formation/@loc, concat($eo:program, '.'))}"/>
        </xsl:if>
      </xsl:for-each>
    </reads>
  </xsl:template>
</xsl:stylesheet>
