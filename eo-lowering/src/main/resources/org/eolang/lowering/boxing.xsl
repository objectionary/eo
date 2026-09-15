<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="boxing" version="2.0">
  <!--
  Here we plant a box on every formation that has a body: one "λ" element
  named "L_box_&lt;n&gt;_&lt;carrier&gt;", appended after everything the formation
  already holds. Nothing else changes, so what comes out is the file the
  parser wrote with one element more in each of those formations, and the
  patch at the far end of the pipeline arrives back at the source by taking
  the same elements away.
  The box is what makes a formation visible to the calculus. A formation
  carrying both a body and a "λ" fires the "λ" when it is evaluated, and the
  body travels with that request, so the engine is asked about the formation
  instead of phino walking into it. An atom is such a request already and
  keeps the "λ" it was written with, since a second one would shadow the only
  thing that says what the atom does. A formation whose "φ" is void, and one
  with no "φ" at all, holds no computation and there is nothing to ask about.
  The number is the name of a formation for every stage that comes after, and
  it is unique across the whole world rather than within a file, because by
  the time the calculus sees them the files are one document. Here the
  formations of one document are numbered in document order, from the "start"
  the caller gives, and the caller counts what it has planted before it runs
  the next file.
  The carrier is the kind of data a formation comes back with. It is read off
  "provides.xml", where "eo:inference" wrote what the program says about
  itself: the row whose id is the locator of the formation says what its type
  reduces to. Five kinds of data have a carrier of their own, the two
  constants of "bool" among them, and everything else is an "object",
  including a formation the tables never saw. Nothing is guessed, since the
  carrier is what the renderer will declare in Java at the end.
  A λ name may hold only letters, digits and underscores, which is why the
  name of a box carries a number and a carrier and nothing else: neither the
  locator of a formation nor the name its author gave it can be spelled in
  one.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  The number the boxes of this document are counted from, so that the first
  one planted here is "start" plus one.
  -->
  <xsl:param name="start" as="xs:integer" select="0"/>
  <!--
  The directory with the tables of "eo:inference", as a URI. The stage that
  runs this stylesheet has made sure the tables are there, so the table is
  opened and never asked about.
  -->
  <xsl:param name="inference" as="xs:string" select="''"/>
  <xsl:variable name="eo:provides" as="document-node()" select="doc(concat(if (ends-with($inference, '/')) then $inference else concat($inference, '/'), 'provides.xml'))"/>
  <!--
  The rows of the table, by the type each one is about. Without the index
  every question about a formation walks the whole table, and the table of
  eo-runtime holds tens of thousands of rows.
  -->
  <xsl:key name="eo:row" match="type" use="@id"/>
  <!--
  The formations of this document that get a box, in document order, by the
  identity of each one. The order is the numbering, and a formation asks this
  list where it stands rather than counting its way back through the
  document, since the question is put once per formation and a count would
  walk the file again for every one of them.
  -->
  <xsl:variable name="eo:boxed" as="xs:string*" select="for $o in //o[eo:boxable(.)] return generate-id($o)"/>
  <!--
  Whether this object is a formation with a body: it copies nothing, it
  declares a "φ" that is not void, and it carries no "λ" of its own.
  -->
  <xsl:function name="eo:boxable" as="xs:boolean">
    <xsl:param name="o" as="element(o)"/>
    <xsl:sequence select="empty($o/@base) and empty($o/o[@name = 'λ']) and exists($o/o[@name = 'φ'][not(@base = '∅')])"/>
  </xsl:function>
  <!--
  The kind of data the formation with this locator comes back with, as the
  tables have it. A type nobody wrote a row about, and one that reduces to
  anything but data, is an "object": the renderer can say nothing narrower
  about it in Java, and saying something narrower on a guess would be a lie
  the whole pipeline then stands on.
  -->
  <xsl:function name="eo:carrier" as="xs:string">
    <xsl:param name="loc" as="xs:string?"/>
    <xsl:variable name="reduced" as="xs:string" select="string(key('eo:row', $loc, $eo:provides)[1]/@reduced)"/>
    <xsl:sequence select="if ($reduced = ('Φ.bool', 'Φ.true', 'Φ.false')) then 'bool' else if ($reduced = ('Φ.number', 'Φ.string', 'Φ.bytes', 'Φ.tuple')) then substring-after($reduced, 'Φ.') else 'object'"/>
  </xsl:function>
  <!--
  The box goes in last, after every binding the formation was written with,
  because the patch takes it off again and the file has to come back exactly
  as it was.
  -->
  <xsl:template match="o[eo:boxable(.)]">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <o name="λ">
        <xsl:value-of select="concat('L_box_', $start + index-of($eo:boxed, generate-id()), '_', eo:carrier(@loc))"/>
      </o>
    </xsl:copy>
  </xsl:template>
  <!-- Default copying -->
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
