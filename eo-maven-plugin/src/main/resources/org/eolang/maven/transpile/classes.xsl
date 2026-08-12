<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="classes" version="2.0">
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  The XMIR of an object, indented, as the text that "to-java" turns into the
  Javadoc of the generated class. It is one function returning one string,
  rather than a template writing many small pieces of text, because the result
  tree we write into is a DOM: every "xsl:text" or "xsl:value-of" a template
  executes becomes a separate DOM text node, and a template that spelled a
  start tag out piece by piece - indent, "&lt;", name, then five pieces per
  attribute - left tens of thousands of them behind for a single object. Both
  building that many nodes and later reading them back (the DOM has to merge
  adjacent text nodes to answer "text()") cost far more than the string
  concatenation itself, so serializing the biggest objects of "eo-runtime"
  took seconds each and this pass alone was two fifths of the whole transpile
  train (#6664).

  Nesting the recursive call inside the enclosing "concat" means the string of
  a subtree is copied once per level of depth above it, which for the depth of
  a real XMIR is far cheaper than the per-node bookkeeping it replaces:
  returning a sequence of pieces and joining them at the end, instead, is
  three times slower here.
  -->
  <xsl:function name="eo:serialize" as="xs:string">
    <xsl:param name="node" as="element()"/>
    <xsl:param name="indent" as="xs:string"/>
    <xsl:variable name="name" as="xs:string" select="name($node)"/>
    <xsl:variable name="kids" as="element()*" select="$node/*"/>
    <xsl:variable name="content" as="xs:string" select="normalize-space(string($node/text()[1]))"/>
    <!-- An object with neither data nor children is written as a single self-closing tag. -->
    <xsl:variable name="void" as="xs:boolean" select="$content = '' and empty($kids)"/>
    <xsl:variable name="deeper" as="xs:string" select="concat($indent, '  ')"/>
    <xsl:sequence select="concat($indent, '&lt;', $name, string-join(for $a in $node/@* return concat(' ', name($a), '=&quot;', $a, '&quot;'), ''), if ($void) then '/&gt;' else '&gt;', $content, string-join(for $k in $kids return concat('&#10;', eo:serialize($k, $deeper)), ''), if (empty($kids)) then '' else concat('&#10;', $indent), if ($void) then '' else concat('&lt;/', $name, '&gt;'))"/>
  </xsl:function>
  <!--
  An atom is a class only when it holds tests, since it is the tests that need
  a Java class of their own - "to-java" writes no ".java" for it, only its
  tests. Spelled as "not atom, or has a test attribute", because when it is
  not an atom the second half of the question does not need asking.
  -->
  <xsl:template match="object/o[not(eo:atom(.)) or exists(o[eo:test-attr(.)])]" priority="1">
    <xsl:apply-templates select="." mode="class"/>
  </xsl:template>
  <xsl:template match="object/o[@base and @name]" priority="2">
    <xsl:apply-templates select="." mode="class">
      <xsl:with-param name="bound" select="true()"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="o" mode="class">
    <xsl:param name="bound"/>
    <class>
      <xsl:if test="eo:atom(.)">
        <xsl:attribute name="skip-java">true</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="@*"/>
      <xsl:choose>
        <xsl:when test="$bound">
          <xsl:copy-of select="."/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="node()"/>
        </xsl:otherwise>
      </xsl:choose>
      <xmir>
        <xsl:value-of select="eo:serialize(., '')"/>
      </xmir>
    </class>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
