<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="purify" version="2.0">
  <!--
  Here we mark every formation that is safe to cache with @pure="true", so
  that a later step of #5165 can let "PhSticky" keep the result of dataizing
  it instead of working it out again.
  Nothing is worked out here: the tables that "eo:inference" leaves in
  "target/eo/6-inference" already say what a program knows about itself, and
  this stylesheet only reads them. The "inference" parameter names that
  directory, and when it names nothing, or the tables are not there - a build
  that skips the "inference" goal - every question below is unanswerable and
  the stylesheet is an identity transform.
  A formation earns the label when four things are true of it:
  1. Every void it has is filled with a number or a string, and the tables
  have seen it filled. "provides.xml" says what goes in, as the evidence of
  the callers a program happens to have:
  &lt;attr name="port" type="Φ.socket.htons.port" void="true"&gt;
  &lt;witnessed&gt;
  &lt;ref loc="Φ.number"/&gt;
  &lt;/witnessed&gt;
  &lt;/attr&gt;
  A void nobody fills has no "witnessed" at all and does not qualify, and
  neither does one filled with anything wider - another formation, a
  termination, or a choice with a member that is not a number or a string.
  The void a formation written as "[^ x]" declares for "ρ" is left to the
  third rule, since what the table says about it is where the formation was
  attached and not what a caller put in.
  2. No object in its body copies anything from the root of the program,
  since such an object is shared by the whole program and its bytes are not
  part of what a caller passes in. The data literals are the exception: the
  parser compiles "1" into a "Φ.number" applied to a "Φ.bytes", so without
  them even "x.plus 1" would never qualify.
  3. Where the body reads the object the formation is attached to - a base
  that starts with "ξ.ρ" - the read is one of its inputs like any other, and
  "links.xml" must type it as a number or a string. The row is the one about
  the "ξ.ρ" reference itself, which "unroll-bases.xsl" leaves one ".ρ" deeper
  for every dispatch step written after it:
  &lt;type id="Φ.number.plus.φ.ρ"&gt;
  &lt;ref loc="Φ.number"/&gt;
  &lt;/type&gt;
  When the row says so, the read is fine and those bytes simply join the
  cache key at run time. When it is absent or wider, the formation is not
  labeled. A base that climbs twice ("ξ.ρ.ρ") is not labeled either: the
  tables describe the first step out and say nothing about the next one.
  4. It has no "λ" attribute of its own. An atom hides its effects from XMIR
  and waits for the "@Impure" half of #5165. An atom nested inside a
  formation is somebody else's business: copying a formation does not run
  what its attributes hold, so the formation around it is judged on what its
  own body does.
  The label means "pure when the inputs are data" and nothing stronger,
  because what "provides.xml" says about a void is evidence of the callers of
  today and never a contract (see "Witnessed.java"). At run time "PhSticky"
  still asks the actual arguments, and the context where the body reads it,
  what they are before it touches its cache, and passes through when the
  answer is not data.
  Formations are found by their locators rather than by their place in the
  document: "set-locators.xsl" opens the transpile train, so every object
  carries a @loc equal to the id of its row in the tables, and the rows of
  "provides.xml" are the formations of the program and nothing else. That is
  what makes the same questions answerable both here, at the end of the
  train, where a formation has become a "class" or an "abstract", and on the
  XMIR as the parser leaves it, where it is still an "o".
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_specials.xsl"/>
  <!--
  The rows of a table, by the type they are about. Both tables are keyed the
  same way, since both name a type in the @id of a "type" element, and the
  document a row is looked for in is the third argument of "key()". Without
  the index every question about a formation walks the whole table, and the
  tables of eo-runtime hold tens of thousands of rows.
  -->
  <xsl:key name="eo:row" match="type" use="@id"/>
  <!--
  The directory with the tables of "eo:inference", as a URI. Empty when
  nothing was worked out, and then nothing is labeled.
  -->
  <xsl:param name="inference" as="xs:string" select="''"/>
  <xsl:variable name="eo:dir" as="xs:string" select="if ($inference = '' or ends-with($inference, '/')) then $inference else concat($inference, '/')"/>
  <xsl:variable name="eo:provides" as="document-node()?" select="if ($eo:dir != '' and doc-available(concat($eo:dir, 'provides.xml'))) then doc(concat($eo:dir, 'provides.xml')) else ()"/>
  <xsl:variable name="eo:links" as="document-node()?" select="if ($eo:dir != '' and doc-available(concat($eo:dir, 'links.xml'))) then doc(concat($eo:dir, 'links.xml')) else ()"/>
  <!--
  The objects whose bytes a caller may pass in: a number and a string are the
  two kinds of data a formation can be given and still be worth caching by
  what it was given.
  -->
  <xsl:variable name="eo:data" as="xs:string+" select="for $n in ('number', 'string') return concat($eo:program, '.', $n)"/>
  <!--
  The bases the parser writes for a literal. Every one of them is a copy of
  an object of the root, so the rule about the root has to let them through.
  -->
  <xsl:variable name="eo:literals" as="xs:string+" select="for $n in ('number', 'string', 'bytes', 'true', 'false') return concat($eo:program, '.', $n)"/>
  <!-- The base that reads the object the formation is attached to. -->
  <xsl:variable name="eo:climb" as="xs:string" select="concat($eo:xi, '.', $eo:rho)"/>
  <!-- The base that climbs one step further than the tables describe. -->
  <xsl:variable name="eo:twice" as="xs:string" select="concat($eo:xi, '.', $eo:rho, '.', $eo:rho)"/>
  <!--
  Whether this object is a formation the label may go on. The questions are
  asked one at a time, cheapest first, and the two that walk the whole body
  of the formation are asked last: an "and" of all six leaves Saxon free to
  work out every one of them, and the walks are then done for every object
  of the program rather than for the few formations the tables let through.
  -->
  <xsl:function name="eo:pure" as="xs:boolean">
    <xsl:param name="f" as="element()"/>
    <xsl:variable name="row" as="element()?" select="key('eo:row', $f/@loc, $eo:provides)"/>
    <xsl:choose>
      <xsl:when test="empty($row) or empty($eo:links) or exists($f/*[@name = $eo:lambda])">
        <xsl:sequence select="false()"/>
      </xsl:when>
      <xsl:when test="not(eo:filled($row))">
        <xsl:sequence select="false()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:closed($f) and eo:attached($f)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- Whether nothing in the body of this formation copies an object of the root. -->
  <xsl:function name="eo:closed" as="xs:boolean">
    <xsl:param name="f" as="element()"/>
    <xsl:sequence select="empty($f/descendant-or-self::*[@base = $eo:program or (starts-with(@base, concat($eo:program, '.')) and not(@base = $eo:literals))])"/>
  </xsl:function>
  <!--
  Whether every void of this row is witnessed as a number or a string. The
  row about "ρ" is passed over, because it does not say what a caller puts
  into the formation but what the formation happened to be attached to, and
  that only matters when the body reads it, which is what "eo:attached" asks
  of the links instead. Asking it here as well would leave every formation
  written as "[^ x]" unlabeled, on the evidence of attachments its body never
  looks at.
  -->
  <xsl:function name="eo:filled" as="xs:boolean">
    <xsl:param name="row" as="element()"/>
    <xsl:sequence select="every $v in $row/attr[@void = 'true'][@name != $eo:rho] satisfies (exists($v/witnessed) and empty($v/witnessed/descendant::*[not(self::union or (self::ref and @loc = $eo:data))]))"/>
  </xsl:function>
  <!-- Whether every read of the context of this formation is typed as data. -->
  <xsl:function name="eo:attached" as="xs:boolean">
    <xsl:param name="f" as="element()"/>
    <xsl:sequence select="every $r in $f/descendant-or-self::*[@base = $eo:climb or starts-with(@base, concat($eo:climb, '.'))] satisfies eo:typed($r)"/>
  </xsl:function>
  <!--
  Whether the tables type this read of the context as a number or a string.
  The row is looked for under the locator the "ξ.ρ" reference has once the
  base is split into one object per dispatch step, which is the locator of
  the reading object itself plus one ".ρ" for every step written after the
  "ξ.ρ".
  -->
  <xsl:function name="eo:typed" as="xs:boolean">
    <xsl:param name="r" as="element()"/>
    <xsl:variable name="steps" as="xs:integer" select="count(tokenize($r/@base, '\.')) - 2"/>
    <xsl:variable name="head" as="xs:string" select="concat($r/@loc, string-join(for $s in 1 to $steps return concat('.', $eo:rho), ''))"/>
    <xsl:variable name="row" as="element()?" select="key('eo:row', $head, $eo:links)"/>
    <xsl:sequence select="not($r/@base = $eo:twice or starts-with($r/@base, concat($eo:twice, '.'))) and exists($row) and empty($row/*[not(self::ref and @loc = $eo:data)])"/>
  </xsl:function>
  <xsl:template match="*[@loc][not(@base)][not(@name = $eo:lambda)][not(text()[normalize-space()])]">
    <xsl:copy>
      <xsl:if test="eo:pure(.)">
        <xsl:attribute name="pure">true</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  <!-- Default copying -->
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
