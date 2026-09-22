<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="entries" version="2.0">
  <!--
  Here we write the entries of the whole build: one object, "l🌵", holding an
  application of every formation that has a body to a symbol for each of its
  voids, so that one run of the calculus over that object folds every
  formation of the world.
  The input of this stylesheet is not a program but a manifest, a list of the
  XMIR files of the build as URIs, and every one of them is opened here
  through "document()". The entries are numbered across the whole world and
  not within a file, because by the time the calculus sees them the files are
  one document, and a number is the only name a later stage has for a
  formation. One transformation reads them all, in the order of the manifest,
  and that order is fixed by the caller, so the three files written here come
  out the same on every run.
  A void is filled with a symbol, a "λ" nobody answers, wrapped in the carrier
  the void holds. It is wrapped and never planted bare, because a formation
  holding nothing but a "λ" carries no attribute at all, and the body of the
  formation is going to dispatch on its inputs. So a number arrives as the
  formation of "number" with the symbol where its bytes would be, and every
  attribute the body takes off it dispatches exactly as it would on a literal.
  What a void holds is read off "provides.xml" and worked out nowhere here.
  The "holds" attribute is the annotation of the source where there is one,
  and otherwise the one type the witnesses of the void agree on, which
  "eo:inference" has already chased through its links. Only that attribute is
  read: chasing anything from here would be a second, worse copy of a job
  another module does, and a void that module says nothing about is left
  unfilled on purpose. The body then reaches ⊥ where it reads that void, the
  entry is a taint, and the formation stays in EO exactly as it was written.
  The "ρ" of a nested formation is a void like the others, but it cannot be
  bound by name: XMIR has no "as" of "ρ". So a formation whose "ρ" is filled
  is written as a dispatch off the planted object, "⟨𝜎5⟩.minus(…)" rather than
  "Φ.number.minus(…)", which binds the same thing the way the calculus does.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  The directory with the tables of "eo:inference", as a URI. The stage that
  runs this stylesheet has made sure the tables are there, so the table is
  opened and never asked about.
  -->
  <xsl:param name="inference" as="xs:string" select="''"/>
  <xsl:variable name="eo:provides" as="document-node()" select="document(concat(if (ends-with($inference, '/')) then $inference else concat($inference, '/'), 'provides.xml'))"/>
  <!--
  The rows of the table, by the type each one is about. Without the index
  every question about a formation walks the whole table, and the table of
  eo-runtime holds tens of thousands of rows.
  -->
  <xsl:key name="eo:type" match="type" use="@id"/>
  <!--
  Every formation of the build that deserves an entry, in the order of the
  manifest and, inside a file, in document order. That order is the
  numbering, and it is why the manifest is sorted before it arrives.
  -->
  <xsl:variable name="eo:formations" as="element(o)*" select="for $s in /sources/source return document($s)//o[eo:entry(.)]"/>
  <!--
  Whether this object is a formation with a body: it copies nothing, it
  carries no "λ" of its own, and it declares a "φ" that is not void. A
  package object has no "φ" and an atom has a "λ", so neither is here.
  A formation whose locator holds an α-index is out too, because an entry
  names its formation by dispatching to it and an argument that was written
  in a place rather than under a name cannot be dispatched to at all.
  -->
  <xsl:function name="eo:entry" as="xs:boolean">
    <xsl:param name="o" as="element(o)"/>
    <xsl:sequence select="empty($o/@base) and empty($o/o[@name = 'λ']) and exists($o/o[@name = 'φ'][not(@base = '∅')]) and not(matches(string($o/@loc), '\.α[0-9]'))"/>
  </xsl:function>
  <!--
  What the named void of the type with this locator holds, with the trailing
  "?" of a maybe-⊥ annotation dropped, or an empty string where the tables
  say nothing.
  -->
  <xsl:function name="eo:holds" as="xs:string">
    <xsl:param name="loc" as="xs:string"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="replace(string(key('eo:type', $loc, $eo:provides)[1]/attr[@name = $name][@void = 'true'][1]/@holds), '\?$', '')"/>
  </xsl:function>
  <!--
  The carrier of what a void holds: one of the five kinds of data the
  renderer can declare in Java, "object" for anything else, and an empty
  string where the tables say nothing at all.
  -->
  <xsl:function name="eo:carrier" as="xs:string">
    <xsl:param name="holds" as="xs:string"/>
    <xsl:sequence select="if ($holds = 'Φ.number') then 'number' else if ($holds = 'Φ.string') then 'string' else if ($holds = 'Φ.bytes') then 'bytes' else if ($holds = ('Φ.bool', 'Φ.true', 'Φ.false')) then 'bool' else if ($holds = 'Φ.tuple') then 'tuple' else if ($holds = '') then '' else 'object'"/>
  </xsl:function>
  <!--
  The plan for one void: what is planted in it and which symbols that
  planting spends. A carrier is planted as the library shapes that datum, a
  tuple as its three attributes, and any other object as itself applied to
  its own voids. That last step goes one level down and no further, so the
  plan of the whole build stays finite and a void of a void of an object is
  left unfilled.
  -->
  <xsl:function name="eo:plant" as="element()">
    <xsl:param name="holds" as="xs:string"/>
    <xsl:param name="path" as="xs:string"/>
    <xsl:param name="deep" as="xs:boolean"/>
    <xsl:variable name="carrier" as="xs:string" select="eo:carrier($holds)"/>
    <xsl:choose>
      <xsl:when test="$carrier = ('number', 'string', 'bytes', 'bool')">
        <plant carrier="{$carrier}">
          <sym path="{$path}" carrier="{$carrier}"/>
        </plant>
      </xsl:when>
      <xsl:when test="$carrier = 'tuple'">
        <plant carrier="tuple">
          <sym path="{concat($path, '.length')}" carrier="number"/>
          <sym path="{concat($path, '.head')}" carrier="object"/>
          <sym path="{concat($path, '.tail')}" carrier="tuple"/>
        </plant>
      </xsl:when>
      <xsl:when test="$carrier = 'object' and $deep">
        <plant carrier="object" base="{$holds}">
          <xsl:for-each select="key('eo:type', $holds, $eo:provides)[1]/attr[@void = 'true'][not(@name = 'ρ')]">
            <xsl:variable name="held" as="xs:string" select="replace(string(@holds), '\?$', '')"/>
            <xsl:choose>
              <xsl:when test="$held = ''">
                <hole/>
              </xsl:when>
              <xsl:otherwise>
                <arg name="{@name}">
                  <xsl:sequence select="eo:plant($held, concat($path, '.', @name), false())"/>
                </arg>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
        </plant>
      </xsl:when>
      <xsl:otherwise>
        <hole/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The power of two, as a whole number, since XSLT 2.0 has no such function
  and the bytes of a number are made of them.
  -->
  <xsl:function name="eo:two" as="xs:integer">
    <xsl:param name="k" as="xs:integer"/>
    <xsl:sequence select="if ($k = 0) then 1 else 2 * eo:two($k - 1)"/>
  </xsl:function>
  <!--
  The whole binary logarithm of a positive whole number, the exponent the
  double of it carries.
  -->
  <xsl:function name="eo:log" as="xs:integer">
    <xsl:param name="n" as="xs:integer"/>
    <xsl:sequence select="if ($n lt 2) then 0 else 1 + eo:log($n idiv 2)"/>
  </xsl:function>
  <!--
  A whole number written in hexadecimal, padded on the left to the given
  count of digits.
  -->
  <xsl:function name="eo:hex" as="xs:string">
    <xsl:param name="v" as="xs:integer"/>
    <xsl:param name="digits" as="xs:integer"/>
    <xsl:sequence select="if ($digits = 0) then '' else concat(eo:hex($v idiv 16, $digits - 1), substring('0123456789ABCDEF', ($v mod 16) + 1, 1))"/>
  </xsl:function>
  <!--
  The eight bytes of a positive whole number, as the parser writes the data
  of a literal: the IEEE 754 double of it, sign zero, the exponent biased by
  1023, and the mantissa without its leading one.
  -->
  <xsl:function name="eo:bytes" as="xs:string">
    <xsl:param name="n" as="xs:integer"/>
    <xsl:variable name="log" as="xs:integer" select="eo:log($n)"/>
    <xsl:variable name="hex" as="xs:string" select="eo:hex((1022 + $log) * eo:two(52) + $n * eo:two(52 - $log), 16)"/>
    <xsl:sequence select="string-join(for $i in 1 to 8 return substring($hex, $i * 2 - 1, 2), '-')"/>
  </xsl:function>
  <!--
  The plan of the whole build, one entry per formation, before a single
  symbol is numbered. The symbols are numbered afterwards by where they stand
  in this tree, which is why the planting is worked out once and written down
  rather than worked out again for each of the three files.
  -->
  <xsl:variable name="eo:plan" as="element(plan)">
    <plan>
      <xsl:for-each select="$eo:formations">
        <xsl:variable name="loc" as="xs:string" select="string(@loc)"/>
        <xsl:variable name="voids" as="element(o)*" select="o[@base = '∅']"/>
        <xsl:variable name="held" as="xs:string" select="if (exists($voids[@name = 'ρ'])) then eo:holds($loc, 'ρ') else ''"/>
        <entry n="{position()}" loc="{$loc}" base="{if ($held = '') then $loc else concat('.', tokenize($loc, '\.')[last()])}">
          <xsl:if test="exists($voids[@name = 'ρ'])">
            <xsl:choose>
              <xsl:when test="$held = ''">
                <hole/>
              </xsl:when>
              <xsl:otherwise>
                <receiver>
                  <xsl:sequence select="eo:plant($held, 'ρ', true())"/>
                </receiver>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:if>
          <xsl:for-each select="$voids[not(@name = 'ρ')]">
            <xsl:variable name="own" as="xs:string" select="eo:holds($loc, string(@name))"/>
            <xsl:choose>
              <xsl:when test="$own = ''">
                <hole/>
              </xsl:when>
              <xsl:otherwise>
                <arg name="{@name}">
                  <xsl:sequence select="eo:plant($own, string(@name), true())"/>
                </arg>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
        </entry>
      </xsl:for-each>
    </plan>
  </xsl:variable>
  <!--
  The identity of every symbol of the plan, in the order they stand there.
  The place a symbol takes in this list is its number across the whole world,
  and phino mints its own after the highest one it finds, so a void symbol
  and a minted one are never spelled alike.
  -->
  <xsl:variable name="eo:symbols" as="xs:string*" select="for $s in $eo:plan//sym return generate-id($s)"/>
  <!--
  The three files, in one document, since one reading of the build has to
  answer for all of them: the XMIR of the entries, the table of the symbols,
  and the table of the formations behind the numbers.
  -->
  <xsl:template match="/">
    <planted entries="{count($eo:plan/entry)}" symbols="{count($eo:plan//sym)}" unfilled="{count($eo:plan//hole)}">
      <object author="eo-lowering">
        <o name="l🌵">
          <o name="mark">
            <o base="∅" name="n"/>
            <o base="∅" name="v"/>
            <o name="λ">L_entry</o>
          </o>
          <o name="root">
            <o base="∅" name="v"/>
            <o name="λ">L_root</o>
          </o>
          <xsl:apply-templates select="$eo:plan/entry" mode="eo:xmir"/>
        </o>
      </object>
      <voids>
        <xsl:for-each select="$eo:plan//sym">
          <xsl:value-of select="concat('𝜎', position(), '&#9;', ancestor::entry/@n, '&#9;', @path, '&#9;', @carrier, '&#10;')"/>
        </xsl:for-each>
      </voids>
      <entries>
        <xsl:for-each select="$eo:plan/entry">
          <xsl:value-of select="concat(@n, '&#9;', @loc, '&#10;')"/>
        </xsl:for-each>
      </entries>
    </planted>
  </xsl:template>
  <!-- One entry: the formation, applied to what was planted, wrapped in the mark. -->
  <xsl:template match="entry" mode="eo:xmir">
    <o base="Φ.l🌵.mark" name="e{@n}">
      <o as="n" base="Φ.number">
        <o as="α0" base="Φ.bytes">
          <o as="α0">
            <xsl:value-of select="eo:bytes(xs:integer(@n))"/>
          </o>
        </o>
      </o>
      <o as="v" base="{@base}">
        <xsl:apply-templates select="receiver|arg" mode="eo:xmir"/>
      </o>
    </o>
  </xsl:template>
  <!-- The receiver of a dispatch stands first and carries no name. -->
  <xsl:template match="receiver" mode="eo:xmir">
    <xsl:apply-templates select="plant" mode="eo:xmir"/>
  </xsl:template>
  <!-- An argument is what was planted, under the name of the void it fills. -->
  <xsl:template match="arg" mode="eo:xmir">
    <xsl:apply-templates select="plant" mode="eo:xmir">
      <xsl:with-param name="as" select="string(@name)"/>
    </xsl:apply-templates>
  </xsl:template>
  <!-- A number and a string carry their symbol where the bytes of a literal are. -->
  <xsl:template match="plant[@carrier = ('number', 'string')]" mode="eo:xmir">
    <xsl:param name="as" as="xs:string" select="''"/>
    <o base="Φ.{@carrier}">
      <xsl:if test="$as != ''">
        <xsl:attribute name="as" select="$as"/>
      </xsl:if>
      <o as="φ" base="Φ.bytes">
        <xsl:apply-templates select="sym" mode="eo:xmir"/>
      </o>
    </o>
  </xsl:template>
  <!-- Bytes carry their symbol themselves. -->
  <xsl:template match="plant[@carrier = 'bytes']" mode="eo:xmir">
    <xsl:param name="as" as="xs:string" select="''"/>
    <o base="Φ.bytes">
      <xsl:if test="$as != ''">
        <xsl:attribute name="as" select="$as"/>
      </xsl:if>
      <xsl:apply-templates select="sym" mode="eo:xmir"/>
    </o>
  </xsl:template>
  <!--
  A bool carries its symbol inside the fork of its "if", because "bool.eo"
  routes "and", "or", "not" and dataization through that attribute, and a
  fork is what the run has to see when a branch is taken.
  -->
  <xsl:template match="plant[@carrier = 'bool']" mode="eo:xmir">
    <xsl:param name="as" as="xs:string" select="''"/>
    <o base="Φ.bool">
      <xsl:if test="$as != ''">
        <xsl:attribute name="as" select="$as"/>
      </xsl:if>
      <o as="if">
        <o name="λ">L_fork</o>
        <o base="∅" name="left"/>
        <o base="∅" name="right"/>
        <o name="φ">
          <o name="λ">
            <xsl:value-of select="concat('𝜎', index-of($eo:symbols, generate-id(sym[1])))"/>
          </o>
        </o>
      </o>
    </o>
  </xsl:template>
  <!-- A tuple spends one symbol on each of its three attributes. -->
  <xsl:template match="plant[@carrier = 'tuple']" mode="eo:xmir">
    <xsl:param name="as" as="xs:string" select="''"/>
    <o base="Φ.tuple">
      <xsl:if test="$as != ''">
        <xsl:attribute name="as" select="$as"/>
      </xsl:if>
      <o as="length" base="Φ.number">
        <o as="φ" base="Φ.bytes">
          <xsl:apply-templates select="sym[1]" mode="eo:xmir"/>
        </o>
      </o>
      <o as="head">
        <o name="λ">
          <xsl:value-of select="concat('𝜎', index-of($eo:symbols, generate-id(sym[2])))"/>
        </o>
      </o>
      <o as="tail">
        <o name="λ">
          <xsl:value-of select="concat('𝜎', index-of($eo:symbols, generate-id(sym[3])))"/>
        </o>
      </o>
    </o>
  </xsl:template>
  <!-- Any other object is itself, applied to what was planted in its own voids. -->
  <xsl:template match="plant[@carrier = 'object']" mode="eo:xmir">
    <xsl:param name="as" as="xs:string" select="''"/>
    <o base="{@base}">
      <xsl:if test="$as != ''">
        <xsl:attribute name="as" select="$as"/>
      </xsl:if>
      <xsl:apply-templates select="arg" mode="eo:xmir"/>
    </o>
  </xsl:template>
  <!-- A symbol is a formation of nothing but the "λ" nobody answers. -->
  <xsl:template match="sym" mode="eo:xmir">
    <o as="φ">
      <o name="λ">
        <xsl:value-of select="concat('𝜎', index-of($eo:symbols, generate-id(.)))"/>
      </o>
    </o>
  </xsl:template>
</xsl:stylesheet>
