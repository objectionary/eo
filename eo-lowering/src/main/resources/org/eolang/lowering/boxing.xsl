<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="boxing" version="2.0">
  <!--
  Here we plant a box into every formation the build decided to lower
  through the engine. A boxed formation gets one more binding, an
  "o" named "λ" holding the name the engine answers under, so that
  dispatching onto that formation fires the engine instead of entering
  the body as written.
  The formations to box arrive in the "planted" parameter, split by
  spaces, each the locator and the name of the box split by a tab. The
  build owns that pairing, since the engine has to agree with it from
  its own process, so nothing here invents a name.
  The tests no fragment reaches are cut away at the same time, because
  every one of them ends up in the universe phino reads, and a universe
  it does not need is a universe it pays to parse.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- The boxes, split by spaces, each a locator and a name split by a tab -->
  <xsl:param name="planted" as="xs:string" select="''"/>
  <!-- The locator of the fragment being lowered, or blank for none -->
  <xsl:param name="kept" as="xs:string" select="''"/>
  <xsl:key name="eo:box" match="box" use="@loc"/>
  <xsl:variable name="boxes">
    <xsl:for-each select="tokenize($planted, ' ')[normalize-space()]">
      <box loc="{substring-before(., '&#9;')}" name="{substring-after(., '&#9;')}"/>
    </xsl:for-each>
  </xsl:variable>
  <!--
  Whether the traversal reaches an object at all: it walks down through
  named formations only, exactly as the table of boxes was built, so an
  object under an application or under an anonymous object is none of
  our business.
  -->
  <xsl:function name="eo:reached" as="xs:boolean">
    <xsl:param name="o" as="element(o)"/>
    <xsl:sequence select="empty($o/ancestor::o[@base or empty(@name)])"/>
  </xsl:function>
  <xsl:template match="o[eo:reached(.)][empty(@base)][@name][starts-with(@name, 'p🌵') or starts-with(@name, 'n🌵')][not(starts-with($kept, concat(@loc, '.')))]" priority="1"/>
  <xsl:template match="o[eo:reached(.)][empty(@base)][@name][key('eo:box', @loc, $boxes)]">
    <xsl:copy>
      <xsl:apply-templates select="@*|node() except o[@name='λ']"/>
      <o name="λ">
        <xsl:value-of select="key('eo:box', @loc, $boxes)/@name"/>
      </o>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
