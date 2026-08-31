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
  asks that of every void the row has; a receiver the formation never declared
  has no void to be witnessed, so an empty list of voids answers the question
  before it is put and the label goes on with the receiver never looked at
  (#7613). The cheap half is asked first: a formation that declares "ρ" has
  been judged on it already.
  Only the reads of this very formation count, which is why no formation may
  stand between a read and this one. A nested formation declaring its own
  receiver and reading that says nothing about the formation around it - its
  "ρ" is another attribute of another object - and counting those would
  withhold the label from most of the formations of a program. The formations
  in between are the ancestors of a read that this formation precedes, and
  asking for them that way leaves the ones above this formation alone: a
  program nests hundreds of objects deep, and the question is put to every
  read of every formation the table lets through.
  -->
  <xsl:function name="eo:borrowed" as="xs:boolean">
    <xsl:param name="f" as="element()"/>
    <xsl:param name="row" as="element()"/>
    <xsl:sequence select="empty($row/attr[@void = 'true'][@name = $eo:rho]) and exists($f/descendant::*[@base = $eo:receiver or starts-with(@base, $eo:from-receiver)][empty(ancestor::*[$f &lt;&lt; .][eo:formation(.)])])"/>
  </xsl:function>
  <!--
  Whether every void of this row is witnessed as a number or a string, the
  "ρ" a formation declares for itself among them.
  -->
  <xsl:function name="eo:filled" as="xs:boolean">
    <xsl:param name="row" as="element()"/>
    <xsl:sequence select="every $v in $row/attr[@void = 'true'] satisfies eo:given($v)"/>
  </xsl:function>
  <!--
  Whether this void of a row of "provides.xml" is witnessed as data, and
  nothing else. A void nobody fills has no "witnessed" and does not
  qualify; a "union" qualifies when every member of it does.
  -->
  <xsl:function name="eo:given" as="xs:boolean">
    <xsl:param name="v" as="element()?"/>
    <xsl:sequence select="exists($v) and exists($v/witnessed) and empty($v/witnessed//*[not(name() = 'union' or (name() = 'ref' and @loc = $eo:data))])"/>
  </xsl:function>
  <!--
  Whether this application is decided by the bytes of its own parts. Every
  part it has, and the receiver an implicit dispatch leaves out of them,
  must be decided by bytes, which is what a row of "links.xml" holding one
  "ref" to a data object says:
  &lt;type id="Φ.app.x.ρ"&gt;
  &lt;ref loc="Φ.number"/&gt;
  &lt;/type&gt;
  A part with no row of its own, or one holding anything else, leaves the
  application unlabeled.
  -->
  <xsl:function name="eo:applied" as="xs:boolean">
    <xsl:param name="a" as="element()"/>
    <xsl:variable name="parts" as="element()*" select="$a/o[@loc]"/>
    <xsl:sequence select="exists($eo:links) and exists($parts) and eo:receives($a) and (every $p in $parts satisfies eo:decided(key('eo:row', $p/@loc, $eo:links), ()))"/>
  </xsl:function>
  <!--
  Whether this row of "links.xml" says its object is decided by bytes
  alone. Every row holds one thing, and all four of the ones the table
  carries are answered here: a "data" is a literal; a "ref" to one of the
  data objects is a copy of one; a "ref" to anything else is followed,
  since what that object comes back with is what this one comes back
  with; and a "var" is a void, put to the same question "eo:filled" puts
  to the voids of a formation, a "union" of data among the answers that
  qualify. A "terminator" and an "unknown" answer nothing and the object
  stays undecided.
  The locators already walked through are carried along, so a ring of
  references - "a" that comes from "b" that comes from "a" - is answered
  instead of walked for ever.
  -->
  <xsl:function name="eo:decided" as="xs:boolean">
    <xsl:param name="row" as="element()?"/>
    <xsl:param name="seen" as="xs:string*"/>
    <xsl:choose>
      <xsl:when test="empty($row) or count($row/*) != 1">
        <xsl:sequence select="false()"/>
      </xsl:when>
      <xsl:when test="exists($row/data) or exists($row/ref[@loc = $eo:data])">
        <xsl:sequence select="true()"/>
      </xsl:when>
      <xsl:when test="exists($row/var)">
        <xsl:sequence select="eo:given(eo:void($row/@id))"/>
      </xsl:when>
      <xsl:when test="exists($row/ref) and not($row/ref/@loc = $seen)">
        <xsl:sequence select="eo:decided(key('eo:row', $row/ref/@loc, $eo:links), ($seen, $row/@id))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="false()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The void this locator names, as "provides.xml" holds it: a locator is the
  name of an attribute under the locator of the formation declaring it, so
  the row is the one of everything but the last part of it.
  -->
  <xsl:function name="eo:void" as="element()?">
    <xsl:param name="loc" as="xs:string"/>
    <xsl:variable name="parts" as="xs:string*" select="tokenize($loc, '\.')"/>
    <xsl:variable name="owner" as="xs:string" select="string-join($parts[position() != last()], '.')"/>
    <xsl:variable name="row" as="element()?" select="if (exists($eo:provides) and $owner != '') then key('eo:row', $owner, $eo:provides) else ()"/>
    <xsl:sequence select="$row/attr[@void = 'true'][@name = $parts[last()]]"/>
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
    <xsl:sequence select="not(starts-with($a/@base, $eo:xi)) or eo:decided(key('eo:row', concat($a/@loc, '.', $eo:rho), $eo:links), ())"/>
  </xsl:function>
  <!--
  An application whose parts and receiver are all decided by bytes is
  labeled too, so that the answer it works out is remembered instead of
  being worked out on every read.
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
