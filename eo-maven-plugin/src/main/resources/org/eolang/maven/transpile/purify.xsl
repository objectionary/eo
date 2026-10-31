<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="purify" version="3.0">
  <!--
  Here we mark every formation that is safe to cache with @pure="true", so
  that a later step of #5165 can let "PhSticky" keep the result of dataizing
  it instead of working it out again.
  Nothing is worked out here: the two tables that "eo:inference" leaves in
  its directory already say what a program knows about itself, and this
  stylesheet only reads them. The "inference" parameter names that
  directory, and when it names nothing, or the tables are not there - a
  build that skips the "inference" goal - every question below is
  unanswerable and the stylesheet is an identity transform.
  "facts.xml" says what the text of the program states, and among it, in
  its "objects", the voids every formation declares:
  &lt;type id="Φ.socket.htons"&gt;
  &lt;attr name="port" type="Φ.socket.htons.port" void="true"/&gt;
  &lt;/type&gt;
  "answers.xml" says, for every place of the program, what the copies of
  it the engine walked turned out to be. The labels sit right under the
  row, or in a "set" each when the copies disagree:
  &lt;o loc="Φ.socket.htons.port" copies="2"&gt;
  &lt;set copies="1"&gt;
  &lt;terminator/&gt;
  &lt;is loc="Φ.number"/&gt;
  &lt;/set&gt;
  &lt;set copies="1"&gt;
  &lt;is loc="Φ.number"/&gt;
  &lt;/set&gt;
  &lt;/o&gt;
  A row says its place is decided by bytes when its labels, every set of
  them taken together, with a termination ("terminator"), an open void
  ("open") and a failing read ("fails") left out, are at least one, and
  each of them is a datum ("data") or a copy of "Φ.number", "Φ.string" or
  "Φ.bytes". The three left out are gaps: a copy that stops or waits there
  gives no bytes at all, so it never hands a cache a wrong answer. Anything
  else - another formation, a label the engine could not tell ("unknown") -
  keeps the label off.
  A formation earns the label when three things are true of it:
  1. Every void it declares is filled with a number, a string or bytes:
  the row of the place of the void, "Φ.socket.htons.port" above, is
  decided by bytes, with a bare datum not counted. What a caller puts into
  a void is an object, and "PhSticky" takes an object as a key only when
  it is a number, a string or bytes. A void nobody fills has nothing but
  gaps in its row and does not qualify.
  The "ρ" a formation declares as "[^ x]" is one of those voids, though its
  row is not read: the engine names the copy in "^" by what its "@" gives,
  so the row may say "Φ.number" where "^" holds a decorator of one. A
  declared "^" holds the formation it is written in, which the locator of
  the formation tells, and it qualifies when that is "Φ.number",
  "Φ.string" or "Φ.bytes". That also settles what a body climbing further
  out reads - "ξ.ρ.ρ" is an attribute of whatever came in as "ρ", and once
  that is data everything taken from it is decided by those same bytes.
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
  because what "answers.xml" says about a void is what the callers of today
  put there and never a contract. At run time "PhSticky" still asks the
  actual arguments, and the context where the body reads it, what they are
  before it touches its cache, and passes through when the answer is not
  data.
  Objects are found by their locators rather than by their place in the
  document: "set-locators.xsl" opens the transpile train, so every object
  carries a @loc equal to the name of its place in both tables. That is what
  makes the same questions answerable both here, at the end of the train,
  where a formation has become a "class" or an "abstract", and on the XMIR
  as the parser leaves it, where it is still an "o".
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:import href="/org/eolang/parser/_specials.xsl"/>
  <!--
  The formations of "facts.xml" and the rows of "answers.xml", by the
  locator each one is about. Without the index every question walks a
  whole table, and the tables of eo-runtime hold tens of thousands of rows.
  -->
  <xsl:key name="eo:object" match="objects/type" use="@id"/>
  <xsl:key name="eo:answer" match="answers/o" use="@loc"/>
  <!--
  The directory with the tables of "eo:inference", as a URI. Empty when
  nothing was worked out, and then nothing is labeled.
  -->
  <xsl:param name="inference" as="xs:string" select="''"/>
  <xsl:variable name="eo:dir" as="xs:string" select="if ($inference = '' or ends-with($inference, '/')) then $inference else concat($inference, '/')"/>
  <!--
  The tables, each absent when the directory is not named or does not hold
  it, and then nothing is labeled.
  -->
  <xsl:variable name="eo:facts" as="document-node()?" select="if ($eo:dir != '' and doc-available(concat($eo:dir, 'facts.xml'))) then doc(concat($eo:dir, 'facts.xml')) else ()"/>
  <xsl:variable name="eo:answers" as="document-node()?" select="if ($eo:dir != '' and doc-available(concat($eo:dir, 'answers.xml'))) then doc(concat($eo:dir, 'answers.xml')) else ()"/>
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
  The prefix of a copy of an object of the root, the base of a body reading
  the object it is attached to, and the prefix of a base taking something out
  of that object. All three are spelled out here rather than where they are
  asked about, because the questions below put them to every object of a
  program and a "concat" written inside a predicate is a value the engine is
  free to work out again for every node that predicate sees.
  -->
  <xsl:variable name="eo:root" as="xs:string" select="concat($eo:program, '.')"/>
  <xsl:variable name="eo:receiver" as="xs:string" select="concat($eo:xi, '.', $eo:rho)"/>
  <xsl:variable name="eo:from-receiver" as="xs:string" select="concat($eo:receiver, '.')"/>
  <!--
  Whether this object is a formation the label may go on. The questions are
  asked one at a time, cheapest first, and the one that walks the whole body
  of the formation is asked last: an "and" of all four leaves Saxon free to
  work out every one of them, and the walk is then done for every object of
  the program rather than for the few formations the tables let through. The
  row is looked up inside the branch that knows there are tables to look in,
  since "key()" takes no empty sequence for the document to search.
  -->
  <xsl:function name="eo:pure" as="xs:boolean">
    <xsl:param name="f" as="element()"/>
    <xsl:choose>
      <xsl:when test="empty($eo:facts) or empty($eo:answers) or exists($f/*[@name = $eo:lambda])">
        <xsl:sequence select="false()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="row" as="element()?" select="key('eo:object', $f/@loc, $eo:facts)"/>
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
    <xsl:sequence select="empty($f/descendant-or-self::*[@base = $eo:program or (starts-with(@base, $eo:root) and not(@base = $eo:literals))])"/>
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
  asks that of every void the formation declares; a receiver it never declared
  is not among them, so an empty list of voids answers the question before it
  is put and the label goes on with the receiver never looked at (#7613). The
  cheap half is asked first: a formation that declares "ρ" has been judged on
  it already.
  Only the reads of this very formation count, which is why no formation may
  stand between a read and this one. A nested formation declaring its own
  receiver and reading that says nothing about the formation around it - its
  "ρ" is another attribute of another object - and counting those would
  withhold the label from most of the formations of a program. The formations
  in between are the ancestors of a read that this formation precedes, and
  asking for them that way leaves the ones above this formation alone: a
  program nests hundreds of objects deep, and the question is put to every
  read of every formation the tables let through.
  -->
  <xsl:function name="eo:borrowed" as="xs:boolean">
    <xsl:param name="f" as="element()"/>
    <xsl:param name="row" as="element()"/>
    <xsl:sequence select="empty($row/attr[@void = 'true'][@name = $eo:rho]) and exists($f/descendant::*[@base = $eo:receiver or starts-with(@base, $eo:from-receiver)][empty(ancestor::*[$f &lt;&lt; .][eo:formation(.)])])"/>
  </xsl:function>
  <!--
  Whether every void this row of "facts.xml" declares is filled with a
  number, a string or bytes, the "ρ" a formation declares for itself among
  them.
  -->
  <xsl:function name="eo:filled" as="xs:boolean">
    <xsl:param name="row" as="element()"/>
    <xsl:sequence select="every $v in $row/attr[@void = 'true'] satisfies eo:given($row/@id, $v/@name)"/>
  </xsl:function>
  <!--
  Whether this void of this formation is filled with a number, a string or
  bytes. A declared "ρ" holds the formation the owner is written in, which
  is everything of the locator of the owner but its last part; any other
  void is asked of the row of its place, where a bare datum does not count.
  -->
  <xsl:function name="eo:given" as="xs:boolean">
    <xsl:param name="owner" as="xs:string"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="$name = $eo:rho">
        <xsl:sequence select="replace($owner, '\.[^.]+$', '') = $eo:data"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:bytes(key('eo:answer', concat($owner, '.', $name), $eo:answers), false())"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  Whether this row of "answers.xml" says its place is decided by bytes:
  its labels, right under it or in any of its sets and with the gaps left
  out, are at least one, and each of them is a copy of a data object, or a
  datum when one counts. A row with nothing but gaps, or no row at all,
  answers nothing, and the place stays undecided.
  -->
  <xsl:function name="eo:bytes" as="xs:boolean">
    <xsl:param name="row" as="element()?"/>
    <xsl:param name="datum" as="xs:boolean"/>
    <xsl:variable name="labels" as="element()*" select="$row/(* | set/*)[not(self::set or self::reduced or self::terminator or self::open or self::fails)]"/>
    <xsl:sequence select="exists($labels) and (every $l in $labels satisfies (exists($l/self::is[@loc = $eo:data]) or ($datum and exists($l/self::data))))"/>
  </xsl:function>
  <!--
  Whether this application is decided by the bytes of its own parts. Every
  part it has, and the receiver an implicit dispatch leaves out of them,
  must be decided by bytes, which is what a row of "answers.xml" naming
  nothing but a datum or a data object says:
  &lt;o loc="Φ.app.x.ρ" copies="1"&gt;
  &lt;is loc="Φ.number"/&gt;
  &lt;/o&gt;
  A part with no row of its own, or one naming anything else, leaves the
  application unlabeled, and so does a call that reaches outside its parts.
  -->
  <xsl:function name="eo:applied" as="xs:boolean">
    <xsl:param name="a" as="element()"/>
    <xsl:variable name="parts" as="element()*" select="$a/o[@loc]"/>
    <xsl:sequence select="exists($eo:answers) and exists($parts) and not(eo:outward($a)) and eo:receives($a) and (every $p in $parts satisfies eo:bytes(key('eo:answer', $p/@loc, $eo:answers), true()))"/>
  </xsl:function>
  <!--
  Whether this application calls an object of the root or a termination.
  An object of the root may reach outside the program: "posix.access" asks
  the disk and "console.write" prints. The same bytes given to it twice need
  not get the same answer twice, and a cache would skip the second call. A
  termination gives back no bytes at all, so its cache would never keep an
  answer. The literals never get here, since the template below leaves them
  out.
  -->
  <xsl:function name="eo:outward" as="xs:boolean">
    <xsl:param name="a" as="element()"/>
    <xsl:sequence select="$a/@base = ($eo:bottom, $eo:program) or starts-with($a/@base, $eo:root)"/>
  </xsl:function>
  <!--
  Whether the object this application is attached to is decided by bytes
  too. It is never one of the parts: an application written as "ξ.name"
  leaves its receiver where the line above it put it, and the tables name
  that receiver as the "ρ" of the application itself. An application that
  copies an object of the program has no receiver of its own and the
  question does not arise. Without a row the question has no answer and
  the label stays off, which is what kept "chunk.get" - "read 0 size" over
  a block of memory - from being remembered by its arguments alone.
  -->
  <xsl:function name="eo:receives" as="xs:boolean">
    <xsl:param name="a" as="element()"/>
    <xsl:sequence select="not(starts-with($a/@base, $eo:xi)) or eo:bytes(key('eo:answer', concat($a/@loc, '.', $eo:rho), $eo:answers), true())"/>
  </xsl:function>
  <!--
  An application whose parts and receiver are all decided by bytes, and
  that calls neither an object of the root nor a termination, is labeled
  too, so that the answer it works out is remembered instead of being worked
  out on every read.
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
