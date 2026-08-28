<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="recursion-to-cps" version="2.0">
  <!--
  Here we rewrite every nested formation that calls itself outside of a tail
  position into continuation-passing style, so that afterwards
  "recursion-to-loop.xsl" finds nothing but tail calls in it and the
  recursion runs as a loop at run time (see #5783). Nothing of this is
  visible to an EO programmer: the rewrite happens on the way to Java only.

  A formation "F" gets one more void, "k🌵", the continuation. Every tail
  leaf of its "φ" (in the sense of "eo:tail" from "_recursion.xsl") that is
  not a self-call is handed to the continuation, "ξ.k🌵 (leaf)"; a tail
  self-call passes the continuation on as its last argument; and a leaf that
  leads to a self-call "S" outside of a tail position becomes the tail call
  "ξ.ρ.F (args of S) ξ.c🌵1", where "c🌵1" is a fresh attribute "[^ r🌵]" of
  the formation the leaf sits in, whose "φ" is the leaf with "S" replaced by
  "ξ.r🌵", handed to the continuation of "F" in turn. The continuation is an
  attribute and not an argument written in place, since the "ρ" of an object
  passed as an argument is the object it is passed to, while the "ρ" of an
  attribute is the object it is taken from, and "c🌵1" has to see the copy of
  "F" that made it. A leaf leads to "S" when "S" is inside of it, or inside of an
  attribute of "F" the leaf refers to, directly or through other attributes:
  such an attribute, "^.F (n.minus 1) > prev", moves into "C" as well, and
  the leaf keeps referring to it by name. The leaf moves one formation
  deeper, so every reference in it that escapes the scope of the leaf (a
  "ξ..." with at least as many "ρ" hops as the anonymous formations between
  the leaf and the reference) gets one more "ρ", unless it names an
  attribute that moved along. A leaf leading to two self-calls is rewritten
  twice, the second continuation nesting inside the first, and an attribute
  leading to both lands in the inner one. Every application of "F" outside
  of "F" itself gets the identity "[r🌵] r🌵 > @" as the continuation.

  A reference "ξ.ρ.ρ.F" is resolved the way the runtime resolves it: as the
  attribute "F" of the formation two hops above the formation the reference
  sits in. Names are not unique in a file, since the package objects merge
  many files, each with its own "a🌵16-4"-like names for the ">>" handles.

  The calls to the continuation and the tail self-calls inside a
  continuation are marked @again here, since "recursion-to-loop.xsl" only
  recognises "ξ.ρ.F" at the level of "φ" itself; the markers are consumed
  by "to-java.xsl". At run time the whole chain unwinds inside the "PhLoop"
  of the first copy of "F": each marker throws, the loop takes the "φ" of
  the next copy or continuation, and the Java stack stays flat while the
  result is being built.

  The rewrite takes two passes: the first writes every continuation in
  place, as an argument marked @cont, and the second lifts each of them
  into an attribute of the nearest formation around it, numbered in the
  order of the document, leaving a reference to the attribute behind.

  The price is an eager spine: "F" used to return the leaf at once and walk
  the recursion only as far as the consumer of the leaf forced it, while now
  the loop runs down to the base case before anything is returned. That is
  unobservable when the step condition is pure and the recursion is finite,
  which holds for the standard library; a formation that recurses over an
  infinite lazy structure and never reads the result of the self-call would
  stop terminating.

  Skipped, and left to recurse as they are: a formation whose "φ" leads to
  no self-call outside of a tail position, or does not lead to every
  attribute that holds one (a lazy structure like "range", whose next step
  is read from outside), that has a "λ", that reads a "φ", that is referred
  to other than by an application "ξ(.ρ)*.F", whose "φ" holds a named
  binding, that calls itself from inside a nested formation or from inside
  the arguments of another self-call, or whose application from outside is
  the receiver of a dispatch while one of its attributes moves away.
  -->
  <xsl:import href="/org/eolang/maven/transpile/_recursion.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:variable name="eo:k" select="'k🌵'"/>
  <xsl:variable name="eo:r" select="'r🌵'"/>
  <!--
  The spelling of a reference to the attribute $name of the formation $hops
  levels above the current one: "ξ.ρ.ρ.name" for two hops.
  -->
  <xsl:function name="eo:up" as="xs:string">
    <xsl:param name="name" as="xs:string"/>
    <xsl:param name="hops" as="xs:integer"/>
    <xsl:sequence select="concat('ξ', string-join(for $i in 1 to $hops return '.ρ', ''), '.', $name)"/>
  </xsl:function>
  <!-- How many "ρ" the reference $base climbs before it names anything: 2 for "ξ.ρ.ρ.x" -->
  <xsl:function name="eo:hops" as="xs:integer">
    <xsl:param name="base" as="xs:string"/>
    <xsl:sequence select="if (starts-with($base, 'ξ')) then string-length(replace($base, '^ξ((\.ρ)*)(\..*)?$', '$1')) idiv 2 else 0"/>
  </xsl:function>
  <!--
  The formation named at the head of the reference $ref, "F" in "ξ.ρ.ρ.F"
  or in "ξ.ρ.ρ.F.x": the attribute "F" of the formation as many hops above
  the formation holding $ref as there are "ρ" in it; nothing, when the
  reference is spelled otherwise or no such formation exists.
  -->
  <xsl:function name="eo:formation" as="element()?">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="base" select="string($ref/@base)"/>
    <xsl:variable name="hops" select="eo:hops($base)"/>
    <xsl:sequence select="if (matches($base, '^ξ(\.ρ)*\.[^.]')) then ($ref/ancestor::o[not(@base)][$hops + 1]/o[not(@base) and @name = tokenize($base, '\.')[$hops + 2]])[1] else ()"/>
  </xsl:function>
  <!--
  The attribute of the formation $f the reference $ref names at its head,
  "A" in "ξ.A" from the scope of $f or in "ξ.ρ.A" from one formation below
  it; nothing, when the reference stops elsewhere.
  -->
  <xsl:function name="eo:attribute" as="element()?">
    <xsl:param name="ref" as="element()"/>
    <xsl:param name="f" as="element()"/>
    <xsl:variable name="base" select="string($ref/@base)"/>
    <xsl:variable name="hops" select="eo:hops($base)"/>
    <xsl:sequence select="if (matches($base, '^ξ(\.ρ)*\.[^.]') and $ref/ancestor::o[not(@base)][$hops + 1] is $f) then ($f/o[@name = tokenize($base, '\.')[$hops + 2]])[1] else ()"/>
  </xsl:function>
  <!--
  Every reference to the formation $f, however it is spelled: all of them
  sit inside the parent of $f, since that is where "ξ(.ρ)*.F" climbs to.
  -->
  <xsl:function name="eo:refs" as="element()*">
    <xsl:param name="f" as="element()"/>
    <xsl:sequence select="$f/parent::o//o[@base][matches(@base, concat('^ξ(\.ρ)*\.', $f/@name, '(\.|$)'))][eo:formation(.) is $f]"/>
  </xsl:function>
  <!--
  The attributes of $f the nodes $nodes refer to, directly or through other
  attributes, with $seen guarding against a cycle of attributes.
  -->
  <xsl:function name="eo:used" as="element()*">
    <xsl:param name="nodes" as="element()*"/>
    <xsl:param name="f" as="element()"/>
    <xsl:param name="seen" as="element()*"/>
    <xsl:variable name="attrs" select="(for $ref in $nodes/descendant-or-self::o[starts-with(@base, 'ξ')] return eo:attribute($ref, $f)) except ($seen, $nodes)"/>
    <xsl:sequence select="$attrs, if (exists($attrs)) then eo:used($attrs, $f, ($seen, $attrs)) else ()"/>
  </xsl:function>
  <!--
  The self-calls among $calls the nodes $nodes lead to: the ones among and
  below them, and the ones inside the attributes of $f they use.
  -->
  <xsl:function name="eo:reach" as="element()*">
    <xsl:param name="nodes" as="element()*"/>
    <xsl:param name="f" as="element()"/>
    <xsl:param name="calls" as="element()*"/>
    <xsl:param name="seen" as="element()*"/>
    <xsl:sequence select="($nodes, eo:used($nodes, $f, $seen))/descendant-or-self::o intersect $calls"/>
  </xsl:function>
  <!-- The attributes of $f that lead to a self-call among $calls, and so move into the continuations -->
  <xsl:function name="eo:tainted" as="element()*">
    <xsl:param name="f" as="element()"/>
    <xsl:param name="calls" as="element()*"/>
    <xsl:sequence select="$f/o[not(@name='φ') and not(@base='∅')][exists(eo:reach(., $f, $calls, ()))]"/>
  </xsl:function>
  <!--
  How deep the attribute $a of $f lands: in the continuation of the last
  self-call among $done it leads to.
  -->
  <xsl:function name="eo:place" as="xs:integer">
    <xsl:param name="a" as="element()"/>
    <xsl:param name="f" as="element()"/>
    <xsl:param name="calls" as="element()*"/>
    <xsl:param name="done" as="element()*"/>
    <xsl:sequence select="(max(for $c in eo:reach($a, $f, $calls, ()) return index-of(for $d in $done return generate-id($d), generate-id($c))), 0)[1]"/>
  </xsl:function>
  <!--
  The base $base as it reads $depth continuations deep inside of $f, for a
  reference found $nesting anonymous formations below the moved leaf: a
  "ξ..." reference climbing at least $nesting hops escapes the leaf and
  gets $depth more "ρ", less the depth of the attribute among $tainted it
  names, if any; anything else is left alone.
  -->
  <xsl:function name="eo:hopped" as="xs:string">
    <xsl:param name="base" as="xs:string"/>
    <xsl:param name="nesting" as="xs:integer"/>
    <xsl:param name="depth" as="xs:integer"/>
    <xsl:param name="f" as="element()"/>
    <xsl:param name="calls" as="element()*"/>
    <xsl:param name="done" as="element()*"/>
    <xsl:param name="tainted" as="element()*"/>
    <xsl:variable name="climb" select="eo:hops($base)"/>
    <xsl:variable name="moved" select="if ($climb = $nesting) then $tainted[@name = tokenize($base, '\.')[$climb + 2]] else ()"/>
    <xsl:variable name="hops" select="if (not(starts-with($base, 'ξ')) or $climb &lt; $nesting) then 0 else if (exists($moved)) then $depth - eo:place($moved[1], $f, $calls, $done) else $depth"/>
    <xsl:sequence select="concat(substring($base, 1, 1), string-join(for $i in 1 to $hops return '.ρ', ''), substring($base, 2))"/>
  </xsl:function>
  <!-- Is the formation $f to be rewritten? See the list of the skipped shapes above -->
  <xsl:function name="eo:cps" as="xs:boolean">
    <xsl:param name="f" as="element()"/>
    <xsl:variable name="phi" select="$f/o[@name='φ' and @base]"/>
    <xsl:variable name="calls" select="$f//o[@base = concat('ξ.ρ.', $f/@name)]"/>
    <xsl:sequence select="exists($f/parent::o) and exists($phi) and exists(eo:reach($phi, $f, $calls, ())[not(eo:tail(., $phi))]) and (every $a in eo:tainted($f, $calls) satisfies exists($a intersect eo:used($phi, $f, ()))) and empty($f/o[@name='λ']) and empty($f//o[contains(@base, 'φ')]) and empty($phi//o[@name][not(ancestor::o[not(@base)][. &gt;&gt; $phi])]) and (every $call in $calls satisfies (eo:formation($call) is $f and empty(eo:reach($call/o, $f, $calls, ())))) and (every $ref in eo:refs($f) satisfies (matches($ref/@base, '^ξ(\.ρ)*\.[^.]+$') and (empty($ref/ancestor::o[. is $f]) or exists($ref intersect $calls)) and (empty(eo:tainted($f, $calls)) or empty($ref[not(@as)]/parent::o[starts-with(@base, '.')]))))"/>
  </xsl:function>
  <!--
  The rewritten formation that the reference $ref applies from outside of
  it, or nothing.
  -->
  <xsl:function name="eo:target" as="element()?">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="f" select="eo:formation($ref)"/>
    <xsl:sequence select="if (matches($ref/@base, '^ξ(\.ρ)*\.[^.]+$') and exists($f) and empty($ref/ancestor::o[. is $f]) and eo:cps($f)) then $f else ()"/>
  </xsl:function>
  <!-- The name of the continuation $c among the continuations of the formation it sits in -->
  <xsl:function name="eo:cont" as="xs:string">
    <xsl:param name="c" as="element()"/>
    <xsl:variable name="home" select="$c/ancestor::o[not(@base)][1]"/>
    <xsl:sequence select="concat('c🌵', count($c/preceding::o[@cont='true'][ancestor::o[not(@base)][1] is $home]) + 1)"/>
  </xsl:function>
  <!-- Two passes: the rewrite, then the lifting of the continuations -->
  <xsl:template match="/">
    <xsl:variable name="rewritten">
      <xsl:apply-templates select="node()"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="exists($rewritten//o[@cont='true'])">
        <xsl:apply-templates select="$rewritten/node()" mode="hoist"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="$rewritten/node()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- The formation: one more void, the continuation, and a rewritten φ -->
  <xsl:template match="o[not(@base) and @name][eo:cps(.)]">
    <xsl:variable name="f" select="."/>
    <xsl:variable name="calls" select="$f//o[@base = concat('ξ.ρ.', $f/@name)]"/>
    <xsl:variable name="tainted" select="eo:tainted($f, $calls)"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="o[@base='∅']"/>
      <o base="∅" name="{$eo:k}">
        <xsl:copy-of select="@line|@pos"/>
      </o>
      <xsl:apply-templates select="o[not(@base='∅') and not(@name='φ')] except $tainted"/>
      <xsl:apply-templates select="o[@name='φ']" mode="body">
        <xsl:with-param name="f" select="$f"/>
        <xsl:with-param name="depth" select="0"/>
        <xsl:with-param name="slot" select="concat('α', count(o[@base='∅' and not(@name='ρ')]))"/>
        <xsl:with-param name="calls" select="$calls"/>
        <xsl:with-param name="done" select="()"/>
        <xsl:with-param name="tainted" select="$tainted"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>
  <!-- An application of a rewritten formation from outside: the identity continuation -->
  <xsl:template match="o[@base]">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
      <xsl:call-template name="eo:identity">
        <xsl:with-param name="f" select="eo:target(.)"/>
      </xsl:call-template>
    </xsl:copy>
  </xsl:template>
  <!-- The identity "[r🌵] r🌵 > @" as the continuation of the application of $f, if any -->
  <xsl:template name="eo:identity">
    <xsl:param name="f" as="element()?"/>
    <xsl:if test="exists($f)">
      <o as="α{count($f/o[@base='∅' and not(@name='ρ')])}">
        <xsl:copy-of select="@line|@pos"/>
        <o base="∅" name="{$eo:r}">
          <xsl:copy-of select="@line|@pos"/>
        </o>
        <o base="ξ.{$eo:r}" name="φ">
          <xsl:copy-of select="@line|@pos"/>
        </o>
      </o>
    </xsl:if>
  </xsl:template>
  <!--
  An expression in a tail position of the φ of the formation $f, seen from
  $depth continuations deep, with the self-calls $done already turned into
  continuations: walked down through the branches of an ".if" and the last
  step of a "seq", and rewritten at its leaves.
  -->
  <xsl:template match="o" mode="body">
    <xsl:param name="f" as="element()"/>
    <xsl:param name="depth" as="xs:integer"/>
    <xsl:param name="slot" as="xs:string"/>
    <xsl:param name="calls" as="element()*"/>
    <xsl:param name="done" as="element()*"/>
    <xsl:param name="tainted" as="element()*"/>
    <xsl:param name="root" as="xs:boolean" select="false()"/>
    <xsl:variable name="left" select="eo:reach(., $f, $calls, ()) except $done"/>
    <xsl:variable name="k" select="eo:up($eo:k, $depth)"/>
    <xsl:variable name="head" as="attribute()*">
      <xsl:choose>
        <xsl:when test="$root">
          <xsl:attribute name="name">φ</xsl:attribute>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="@as|@name"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="ends-with(@base, '.if') and o[@as=('α0', 'α1')] and empty(eo:reach(o[not(@as=('α0', 'α1'))], $f, $calls, ()) intersect $left)">
        <xsl:copy>
          <xsl:apply-templates select="@* except (@as, @name)"/>
          <xsl:copy-of select="$head"/>
          <xsl:attribute name="base" select="eo:hopped(@base, 0, $depth, $f, $calls, $done, $tainted)"/>
          <xsl:for-each select="node()">
            <xsl:choose>
              <xsl:when test="self::o[@as=('α0', 'α1')]">
                <xsl:apply-templates select="." mode="body">
                  <xsl:with-param name="f" select="$f"/>
                  <xsl:with-param name="depth" select="$depth"/>
                  <xsl:with-param name="slot" select="$slot"/>
                  <xsl:with-param name="calls" select="$calls"/>
                  <xsl:with-param name="done" select="$done"/>
                  <xsl:with-param name="tainted" select="$tainted"/>
                </xsl:apply-templates>
              </xsl:when>
              <xsl:otherwise>
                <xsl:apply-templates select="." mode="hop">
                  <xsl:with-param name="f" select="$f"/>
                  <xsl:with-param name="depth" select="$depth"/>
                  <xsl:with-param name="calls" select="$calls"/>
                  <xsl:with-param name="done" select="$done"/>
                  <xsl:with-param name="tainted" select="$tainted"/>
                </xsl:apply-templates>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
        </xsl:copy>
      </xsl:when>
      <xsl:when test="@base='Φ.seq' and o[@as='α0' and @base='Φ.tuple']/o[@as='α1'] and empty(eo:reach((o[not(@as='α0')], o[@as='α0']/o[not(@as='α1')]), $f, $calls, ()) intersect $left)">
        <xsl:copy>
          <xsl:apply-templates select="@* except (@as, @name)"/>
          <xsl:copy-of select="$head"/>
          <xsl:for-each select="node()">
            <xsl:choose>
              <xsl:when test="self::o[@as='α0']">
                <xsl:copy>
                  <xsl:apply-templates select="@*"/>
                  <xsl:for-each select="node()">
                    <xsl:choose>
                      <xsl:when test="self::o[@as='α1']">
                        <xsl:apply-templates select="." mode="body">
                          <xsl:with-param name="f" select="$f"/>
                          <xsl:with-param name="depth" select="$depth"/>
                          <xsl:with-param name="slot" select="$slot"/>
                          <xsl:with-param name="calls" select="$calls"/>
                          <xsl:with-param name="done" select="$done"/>
                          <xsl:with-param name="tainted" select="$tainted"/>
                        </xsl:apply-templates>
                      </xsl:when>
                      <xsl:otherwise>
                        <xsl:apply-templates select="." mode="hop">
                          <xsl:with-param name="f" select="$f"/>
                          <xsl:with-param name="depth" select="$depth"/>
                          <xsl:with-param name="calls" select="$calls"/>
                          <xsl:with-param name="done" select="$done"/>
                          <xsl:with-param name="tainted" select="$tainted"/>
                        </xsl:apply-templates>
                      </xsl:otherwise>
                    </xsl:choose>
                  </xsl:for-each>
                </xsl:copy>
              </xsl:when>
              <xsl:otherwise>
                <xsl:apply-templates select="." mode="hop">
                  <xsl:with-param name="f" select="$f"/>
                  <xsl:with-param name="depth" select="$depth"/>
                  <xsl:with-param name="calls" select="$calls"/>
                  <xsl:with-param name="done" select="$done"/>
                  <xsl:with-param name="tainted" select="$tainted"/>
                </xsl:apply-templates>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
        </xsl:copy>
      </xsl:when>
      <xsl:when test="exists(. intersect $left)">
        <xsl:copy>
          <xsl:apply-templates select="@* except (@as, @name)"/>
          <xsl:copy-of select="$head"/>
          <xsl:attribute name="base" select="eo:up($f/@name, $depth + 1)"/>
          <xsl:attribute name="again">true</xsl:attribute>
          <xsl:apply-templates select="node()" mode="hop">
            <xsl:with-param name="f" select="$f"/>
            <xsl:with-param name="depth" select="$depth"/>
            <xsl:with-param name="calls" select="$calls"/>
            <xsl:with-param name="done" select="$done"/>
            <xsl:with-param name="tainted" select="$tainted"/>
          </xsl:apply-templates>
          <o as="{$slot}" base="{$k}">
            <xsl:copy-of select="@line|@pos"/>
          </o>
        </xsl:copy>
      </xsl:when>
      <xsl:when test="exists($left)">
        <xsl:variable name="call" select="$left[1]"/>
        <xsl:variable name="opened" select="($done, $call)"/>
        <o base="{eo:up($f/@name, $depth + 1)}" again="true">
          <xsl:copy-of select="$head"/>
          <xsl:copy-of select="$call/@line|$call/@pos"/>
          <xsl:apply-templates select="$call/node()" mode="hop">
            <xsl:with-param name="f" select="$f"/>
            <xsl:with-param name="depth" select="$depth"/>
            <xsl:with-param name="calls" select="$calls"/>
            <xsl:with-param name="done" select="$done"/>
            <xsl:with-param name="tainted" select="$tainted"/>
          </xsl:apply-templates>
          <o as="{$slot}" cont="true">
            <xsl:copy-of select="$call/@line|$call/@pos"/>
            <o base="∅" name="ρ">
              <xsl:copy-of select="$call/@line|$call/@pos"/>
            </o>
            <o base="∅" name="{$eo:r}">
              <xsl:copy-of select="$call/@line|$call/@pos"/>
            </o>
            <xsl:apply-templates select="$tainted[empty(eo:reach(., $f, $calls, ()) except $opened) and eo:place(., $f, $calls, $opened) = $depth + 1]" mode="hop">
              <xsl:with-param name="f" select="$f"/>
              <xsl:with-param name="depth" select="$depth + 1"/>
              <xsl:with-param name="calls" select="$calls"/>
              <xsl:with-param name="done" select="$opened"/>
              <xsl:with-param name="tainted" select="$tainted"/>
            </xsl:apply-templates>
            <xsl:apply-templates select="." mode="body">
              <xsl:with-param name="f" select="$f"/>
              <xsl:with-param name="depth" select="$depth + 1"/>
              <xsl:with-param name="slot" select="$slot"/>
              <xsl:with-param name="calls" select="$calls"/>
              <xsl:with-param name="done" select="$opened"/>
              <xsl:with-param name="tainted" select="$tainted"/>
              <xsl:with-param name="root" select="true()"/>
            </xsl:apply-templates>
          </o>
        </o>
      </xsl:when>
      <xsl:otherwise>
        <o base="{$k}" again="true">
          <xsl:copy-of select="$head"/>
          <xsl:copy-of select="@line|@pos"/>
          <xsl:copy>
            <xsl:apply-templates select="@* except (@as, @name)"/>
            <xsl:if test="@base">
              <xsl:attribute name="base" select="eo:hopped(@base, 0, $depth, $f, $calls, $done, $tainted)"/>
            </xsl:if>
            <xsl:attribute name="as">α0</xsl:attribute>
            <xsl:apply-templates select="node()" mode="hop">
              <xsl:with-param name="f" select="$f"/>
              <xsl:with-param name="depth" select="$depth"/>
              <xsl:with-param name="calls" select="$calls"/>
              <xsl:with-param name="done" select="$done"/>
              <xsl:with-param name="tainted" select="$tainted"/>
              <xsl:with-param name="nesting" select="if (not(@base) and o) then 1 else 0"/>
            </xsl:apply-templates>
            <xsl:call-template name="eo:identity">
              <xsl:with-param name="f" select="eo:target(.)"/>
            </xsl:call-template>
          </xsl:copy>
        </o>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!--
  A part of a leaf or of a moved attribute of $f on its way $depth
  continuations deep: a self-call among $done becomes the reference to the
  result void of the continuation it opened, every escaping reference climbs
  the hops "eo:hopped" says, and an application of another rewritten
  formation gets the identity.
  -->
  <xsl:template match="o" mode="hop">
    <xsl:param name="f" as="element()"/>
    <xsl:param name="depth" as="xs:integer"/>
    <xsl:param name="calls" as="element()*"/>
    <xsl:param name="done" as="element()*"/>
    <xsl:param name="tainted" as="element()*"/>
    <xsl:param name="nesting" as="xs:integer" select="0"/>
    <xsl:variable name="opened" select="index-of(for $d in $done return generate-id($d), generate-id(.))"/>
    <xsl:choose>
      <xsl:when test="exists($opened)">
        <o base="{eo:up($eo:r, $depth - $opened[1])}">
          <xsl:copy-of select="@as|@name|@line|@pos"/>
        </o>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="@*"/>
          <xsl:if test="@base">
            <xsl:attribute name="base" select="eo:hopped(@base, $nesting, $depth, $f, $calls, $done, $tainted)"/>
          </xsl:if>
          <xsl:apply-templates select="node()" mode="hop">
            <xsl:with-param name="f" select="$f"/>
            <xsl:with-param name="depth" select="$depth"/>
            <xsl:with-param name="calls" select="$calls"/>
            <xsl:with-param name="done" select="$done"/>
            <xsl:with-param name="tainted" select="$tainted"/>
            <xsl:with-param name="nesting" select="if (not(@base) and o) then $nesting + 1 else $nesting"/>
          </xsl:apply-templates>
          <xsl:if test="@base">
            <xsl:call-template name="eo:identity">
              <xsl:with-param name="f" select="eo:target(.)"/>
            </xsl:call-template>
          </xsl:if>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="text()" mode="hop">
    <xsl:copy/>
  </xsl:template>
  <!-- The second pass: a continuation written in place becomes a reference to a lifted one -->
  <xsl:template match="o[@cont='true']" mode="hoist" priority="1">
    <o as="{@as}" base="ξ.{eo:cont(.)}">
      <xsl:copy-of select="@line|@pos"/>
    </o>
  </xsl:template>
  <xsl:template match="o[not(@base)]" mode="hoist">
    <xsl:copy>
      <xsl:apply-templates select="@*" mode="hoist"/>
      <xsl:call-template name="eo:inside"/>
    </xsl:copy>
  </xsl:template>
  <!-- The continuation lifted: an attribute of the formation it was written in -->
  <xsl:template match="o" mode="lift">
    <xsl:copy>
      <xsl:apply-templates select="@* except (@as, @cont)" mode="hoist"/>
      <xsl:attribute name="name" select="eo:cont(.)"/>
      <xsl:call-template name="eo:inside"/>
    </xsl:copy>
  </xsl:template>
  <!-- The children of a formation, followed by the continuations written inside of it -->
  <xsl:template name="eo:inside">
    <xsl:apply-templates select="node()" mode="hoist"/>
    <xsl:apply-templates select=".//o[@cont='true'][ancestor::o[not(@base)][1] is current()]" mode="lift"/>
  </xsl:template>
  <xsl:template match="node()|@*" mode="hoist">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*" mode="hoist"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
