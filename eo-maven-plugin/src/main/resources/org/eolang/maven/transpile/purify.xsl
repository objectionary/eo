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
  A formation earns the label when three things are true of it:
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
  The "ρ" a formation declares as "[^ x]" is one of those voids and is asked
  the same question as the rest: the object a formation is attached to is an
  input it is handed like any other, with no special meaning, so a body that
  reads it reads a number or a string or the formation is not labeled. That
  also settles what a body climbing further out reads - "ξ.ρ.ρ" is an
  attribute of whatever came in as "ρ", and once that is data everything
  taken from it is decided by those same bytes.
  2. No object in its body copies anything from the root of the program,
  since such an object is shared by the whole program and its bytes are not
  part of what a caller passes in. The data literals are the exception: the
  parser compiles "1" into a "Φ.number" applied to a "Φ.bytes", so without
  them even "x.plus 1" would never qualify.
  3. It has no "λ" attribute of its own. An atom hides its effects from XMIR
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
  The rows of the table, by the type each one is about. Without the index
  every question about a formation walks the whole table, and the table of
  eo-runtime holds tens of thousands of rows.
  -->
  <xsl:key name="eo:row" match="type" use="@id"/>
  <!--
  The directory with the tables of "eo:inference", as a URI. Empty when
  nothing was worked out, and then nothing is labeled.
  -->
  <xsl:param name="inference" as="xs:string" select="''"/>
  <xsl:variable name="eo:dir" as="xs:string" select="if ($inference = '' or ends-with($inference, '/')) then $inference else concat($inference, '/')"/>
  <xsl:variable name="eo:provides" as="document-node()?" select="if ($eo:dir != '' and doc-available(concat($eo:dir, 'provides.xml'))) then doc(concat($eo:dir, 'provides.xml')) else ()"/>
  <!--
  Where the table of links is expected to be.
  -->
  <xsl:variable name="eo:links-file" as="xs:string" select="concat($eo:dir, 'links.xml')"/>
  <!--
  The table that says what every part of an application is, by the locator of
  that part. Absent for the same reasons "provides.xml" may be absent, and
  then no application is labeled.
  -->
  <xsl:variable name="eo:links" as="document-node()?" select="if ($eo:dir != '' and doc-available($eo:links-file)) then doc($eo:links-file) else ()"/>
  <!--
  The objects whose bytes a caller may pass in: a number, a string and a bytes
  are the kinds of data a formation can be given and still be worth caching by
  what it was given, since each one of them is decided by its bytes alone.
  -->
  <xsl:variable name="eo:data" as="xs:string+" select="for $n in ('number', 'string', 'bytes') return concat($eo:program, '.', $n)"/>
  <!--
  The bases the parser writes for a literal. Every one of them is a copy of
  an object of the root, so the rule about the root has to let them through.
  -->
  <xsl:variable name="eo:literals" as="xs:string+" select="for $n in ('number', 'string', 'bytes', 'true', 'false') return concat($eo:program, '.', $n)"/>
  <!--
  Whether this object is a formation the label may go on. The questions are
  asked one at a time, cheapest first, and the one that walks the whole body
  of the formation is asked last: an "and" of all four leaves Saxon free to
  work out every one of them, and the walk is then done for every object of
  the program rather than for the few formations the table lets through. The
  row is looked up inside the branch that knows there is a table to look in,
  since "key()" takes no empty sequence for the document to search.
  -->
  <xsl:function name="eo:pure" as="xs:boolean">
    <xsl:param name="f" as="element()"/>
    <xsl:choose>
      <xsl:when test="empty($eo:provides) or exists($f/*[@name = $eo:lambda])">
        <xsl:sequence select="false()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="row" as="element()?" select="key('eo:row', $f/@loc, $eo:provides)"/>
        <xsl:choose>
          <xsl:when test="empty($row) or not(eo:filled($row))">
            <xsl:sequence select="false()"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:sequence select="eo:closed($f) and not(eo:borrowed($f, $row))"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- Whether nothing in the body of this formation copies an object of the root. -->
  <xsl:function name="eo:closed" as="xs:boolean">
    <xsl:param name="f" as="element()"/>
    <xsl:sequence select="empty($f/descendant-or-self::*[@base = $eo:program or (starts-with(@base, concat($eo:program, '.')) and not(@base = $eo:literals))])"/>
  </xsl:function>
  <!--
  Whether this element is a formation, the same shape the template below
  matches: an object with a locator of its own, copying nothing, that is
  neither the "λ" of an atom nor a piece of data.
  -->
  <xsl:function name="eo:formation" as="xs:boolean">
    <xsl:param name="e" as="element()"/>
    <xsl:sequence select="exists($e/@loc) and empty($e/@base) and not($e/@name = $eo:lambda) and not($e/text()[normalize-space()])"/>
  </xsl:function>
  <!--
  Whether this formation reads the object it is attached to without declaring
  it. Rule 1 above judges the receiver like any other input, and "eo:filled"
  asks that of every void the row has; a receiver the formation never declared
  has no void to be witnessed, so an empty list of voids answers the question
  before it is put and the label goes on with the receiver never looked at
  (#7613). The cheap half is asked first: a formation that declares "ρ" has
  been judged on it already.
  Only the reads of this very formation count, which is why the nearest
  formation around each read has to be this one. A nested formation declaring
  its own receiver and reading that says nothing about the formation around
  it - its "ρ" is another attribute of another object - and counting those
  would withhold the label from most of the formations of a program.
  -->
  <xsl:function name="eo:borrowed" as="xs:boolean">
    <xsl:param name="f" as="element()"/>
    <xsl:param name="row" as="element()"/>
    <xsl:sequence select="empty($row/attr[@void = 'true'][@name = $eo:rho]) and exists($f/descendant::*[(@base = concat($eo:xi, '.', $eo:rho) or starts-with(@base, concat($eo:xi, '.', $eo:rho, '.'))) and (ancestor::*[eo:formation(.)][1] is $f)])"/>
  </xsl:function>
  <!--
  Whether every void of this row is witnessed as a number or a string, the
  "ρ" a formation declares for itself among them.
  -->
  <xsl:function name="eo:filled" as="xs:boolean">
    <xsl:param name="row" as="element()"/>
    <xsl:sequence select="every $v in $row/attr[@void = 'true'] satisfies (exists($v/witnessed) and empty($v/witnessed/descendant::*[not(self::union or (self::ref and @loc = $eo:data))]))"/>
  </xsl:function>
  <!--
  Whether this application is decided by the bytes of its own parts. Every
  part it has, the receiver among them, must be a copy of data, which is what
  a row of "links.xml" holding one "ref" to a data object says:
  &lt;type id="Φ.app.x.ρ"&gt;
  &lt;ref loc="Φ.number"/&gt;
  &lt;/type&gt;
  A part with no row of its own, or one holding anything else, leaves the
  application unlabeled.
  -->
  <xsl:function name="eo:applied" as="xs:boolean">
    <xsl:param name="a" as="element()"/>
    <xsl:sequence select="exists($eo:links) and exists($a/o[@loc]) and (every $p in $a/o[@loc] satisfies eo:copies-data(key('eo:row', $p/@loc, $eo:links)))"/>
  </xsl:function>
  <!--
  Whether this row of "links.xml" says its object is data. A row holds one
  answer, and three of the kinds it may hold are data: a copy of a data
  object outright, a copy of something the tables say is data anyway
  ("eo:data-target"), and a choice all of whose members are one of those.
  A row with no answer, or one holding anything else, leaves the application
  unlabeled.
  -->
  <xsl:function name="eo:copies-data" as="xs:boolean">
    <xsl:param name="row" as="element()?"/>
    <xsl:sequence select="exists($row) and count($row/*) = 1 and (exists($row/ref[@loc = $eo:data]) or (exists($row/ref) and eo:data-target($row/ref/@loc)) or (exists($row/union) and eo:chosen($row/union)))"/>
  </xsl:function>
  <!--
  Whether every member of this choice is data. A member is written the same
  way the answer of a row is, so the same two questions are put to it: the
  bytes of a literal, which the table writes as "data", and a copy of
  something known to be data.
  -->
  <xsl:function name="eo:chosen" as="xs:boolean">
    <xsl:param name="union" as="element()"/>
    <xsl:sequence select="exists($union/*) and (every $m in $union/* satisfies (name($m) = 'data' or (name($m) = 'ref' and ($m/@loc = $eo:data or eo:data-target($m/@loc)))))"/>
  </xsl:function>
  <!--
  Whether the object this locator names is data, judged by what the tables
  say about it rather than by its own row of "links.xml": a formation that
  comes back with a data object, and a void the program is only ever seen
  filling with data. The second question is the one "eo:filled" puts to the
  voids of a formation, asked here of one void named from the outside.
  -->
  <xsl:function name="eo:data-target" as="xs:boolean">
    <xsl:param name="loc" as="xs:string"/>
    <xsl:sequence select="exists($eo:provides) and ((some $t in key('eo:row', $loc, $eo:provides) satisfies $t/@returns = $eo:data) or (some $v in $eo:provides//attr[@void = 'true'][@type = $loc] satisfies (exists($v/witnessed) and (every $e in $v/witnessed//* satisfies (name($e) = 'union' or (name($e) = 'ref' and $e/@loc = $eo:data))))))"/>
  </xsl:function>
  <!--
  An application whose parts are all data is labeled too, so that the answer
  it works out is remembered instead of being worked out on every read.
  -->
  <xsl:template match="*[@loc][@base][not(@base = $eo:literals)][o[@loc]]">
    <xsl:copy>
      <xsl:if test="eo:applied(.)">
        <xsl:attribute name="pure">true</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
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
