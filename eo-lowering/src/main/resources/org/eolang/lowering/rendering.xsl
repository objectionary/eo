<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:map="http://www.w3.org/2005/xpath-functions/map" xmlns:err="http://www.w3.org/2005/xqt-errors" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo err map xs" id="rendering" version="3.0">
  <!--
  Here the protocol of one entry becomes the Java class of one atom. The
  input is the protocol phino wrote for that entry, and the output is either
  "atom", holding the path of the file under the directory of generated
  sources, the Java to put there, and how many voids it reads, statements it
  computes, and conditions it forks on, or "taint", holding why there is none.
  The protocol holds one path for every way the run of the entry went. A run
  forks where a λ function branches on a value nobody worked out. phino
  morphs both sides first, and when they come to one term up to their
  symbols it joins them into a fresh symbol that stands for either, and the
  run goes on as one. Otherwise the run splits at the fork into two paths:
  the first goes on with the left side, and the second starts at the fork
  with the right side, in the state the run was in there, so it holds only
  what happened after the fork. phino writes the paths in the order their
  forks opened them: the first path is the run itself, and every fork a
  path meets afresh, taking the left side, opens the next path not written
  yet. So every path is a list of decisions, the ones it took over from the
  path it left and the ones it made itself, and all the paths together are
  a tree of ifs. The atom is that tree: the statements every path reads
  stand before the first if, every if asks the condition of one fork, and
  every leaf of the tree returns what its path came to, or throws the message
  its path stopped at, when the side the path took is a copy of "throw",
  which every "T" became before the world was merged. A path that came to
  neither a root nor a throw makes the entry a taint, and so does a side that
  phino never ran, and so does a throw whose message is no string literal.
  The protocol says what fired and off which symbols, and a symbol is all a
  Java local is: a void is read off the object the atom lives in, a minted
  symbol is one statement under the operation of its λ, a symbolized one is
  a literal, a joined one is a blank final that each branch of an if
  assigns, and a deferred one is the copy of an object of the entry with
  its arguments bound, which phino left for the run to work out. A loop
  phino cut gets no symbol, so a path that met one makes the entry a taint.
  The copy is made where phino defers it, and dataized only where a λ reads
  it as an operand, since phino read it as data right there. The root of a path is
  returned inside the object the protocol names around it, when the answer
  of "L_entry" for the body is an object of the world applied to its "φ"
  alone, since such an object dataizes through its "φ" and the data of the
  root is the data of that "φ": a string, for example, is "Φ.string" applied
  to its bytes again, and a number is what "Data.ToPhi" makes of its double.
  Otherwise the root is returned as the object it is when it is a void or
  a copy, and bare when it is data the atom computed or a constant the path
  came down to. Bare data loses the object around it, so then the tables of
  "eo:inference" are asked what the body returns, and an entry whose body is
  not a number, a bool or bytes is a taint: a string would lose every
  attribute of its own, and so would an "i16". A statement is placed as
  deep inside the ifs as all of its readers let it, so that what one path
  alone needs is computed on that path alone. The symbols are walked from
  the highest number down, since phino mints a symbol only after the symbols
  it is made of, so by the time a symbol is reached every symbol that reads
  it has already said where it is read. An entry whose protocol defines one
  symbol twice is a taint, since phino promises to mint every symbol once
  and the atom could not tell which of the two values a reader of the symbol
  means.
  A taint is raised as an error and caught once, at the top, since there is
  no half of an atom worth writing.
  The class is named the way "_java-names.xsl" of the transpiler names every
  class, and its functions are copied here, because the transpiler names
  every reference to the atom by that rule and javac finds the class only
  under that one name.
  -->
  <xsl:include href="/org/eolang/lowering/_returns.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- The number of the entry, the one "entries.tsv" gives it. -->
  <xsl:param name="number" as="xs:string" select="''"/>
  <!-- The locator of the formation of the entry. -->
  <xsl:param name="locator" as="xs:string" select="''"/>
  <!-- The locator of the top object of the source the formation is in. -->
  <xsl:param name="top" as="xs:string" select="''"/>
  <!-- The XMIR of that source, as a URI. -->
  <xsl:param name="source" as="xs:string" select="''"/>
  <!-- The table of voids the planting wrote, as a URI. -->
  <xsl:param name="voids" as="xs:string" select="''"/>
  <!--
  The directory with the tables of "eo:inference", as a URI. The tables are
  opened only when the root of a path is data, since the table of
  eo-runtime is large and nothing else asks about the body.
  -->
  <xsl:param name="inference" as="xs:string" select="''"/>
  <xsl:variable name="eo:tables" as="xs:string" select="if (ends-with($inference, '/')) then $inference else concat($inference, '/')"/>
  <xsl:variable name="eo:provides" as="document-node()" select="document(concat($eo:tables, 'provides.xml'))"/>
  <xsl:variable name="eo:links" as="document-node()" select="document(concat($eo:tables, 'links.xml'))"/>
  <xsl:variable name="eo:atoms" as="document-node()" select="document(concat($eo:tables, 'atoms.xml'))"/>
  <xsl:variable name="eo:alpha" select="'α'"/>
  <xsl:variable name="eo:phi" select="'φ'"/>
  <!--
  What every λ the atom can spell is: the type of what it mints, the types of
  its operands in the order phino lists them, and the Java of the operation
  with a numbered hole for each operand. Out of its range, the atom of
  "L_bytes_slice" returns the "cant-slice" its caller binds, which the
  protocol does not hold. No object of eo-runtime binds it, so reading it
  fails, and "Slice" fails the same way. A caller that binds it loses its
  fallback, and this is accepted.
  -->
  <xsl:variable name="eo:operations" as="element()*">
    <op λ="L_number_plus" type="double" args="double double">⟨1⟩ + ⟨2⟩</op>
    <op λ="L_number_times" type="double" args="double double">⟨1⟩ * ⟨2⟩</op>
    <op λ="L_number_div" type="double" args="double double">⟨1⟩ / ⟨2⟩</op>
    <op λ="L_number_gt" type="boolean" args="double double">⟨1⟩ &gt; ⟨2⟩</op>
    <op λ="L_bytes_size" type="double" args="byte[]">⟨1⟩.length</op>
    <op λ="L_bytes_eq" type="boolean" args="byte[] byte[]">java.util.Arrays.equals(⟨1⟩, ⟨2⟩)</op>
    <op λ="L_bytes_and" type="byte[]" args="byte[] byte[]">new BytesOf(⟨1⟩).and(new BytesOf(⟨2⟩)).take()</op>
    <op λ="L_bytes_or" type="byte[]" args="byte[] byte[]">new BytesOf(⟨1⟩).or(new BytesOf(⟨2⟩)).take()</op>
    <op λ="L_bytes_not" type="byte[]" args="byte[]">new BytesOf(⟨1⟩).not().take()</op>
    <op λ="L_bytes_right" type="byte[]" args="byte[] double">new BytesOf(⟨1⟩).shift(new Int("b", new Data.ToPhi(⟨2⟩)).it()).take()</op>
    <op λ="L_bytes_concat" type="byte[]" args="byte[] byte[]">java.nio.ByteBuffer.allocate(⟨1⟩.length + ⟨2⟩.length).put(⟨1⟩).put(⟨2⟩).array()</op>
    <op λ="L_bytes_slice" type="byte[]" args="byte[] double double">new Slice(⟨1⟩, ⟨2⟩, ⟨3⟩).delta()</op>
    <op λ="L_dataized" type="byte[]" args="byte[]">⟨1⟩</op>
    <op λ="L_os_name" type="byte[]" args="">System.getProperty("os.name").getBytes(java.nio.charset.StandardCharsets.UTF_8)</op>
  </xsl:variable>
  <xsl:key name="eo:minted" match="minted" use="@symbol"/>
  <xsl:key name="eo:joined" match="joined" use="@symbol"/>
  <xsl:key name="eo:symbolized" match="symbolized" use="@symbol"/>
  <xsl:key name="eo:deferred" match="deferred" use="@symbol"/>
  <!-- The voids of this entry, by symbol, each as its path and its carrier. -->
  <xsl:variable name="eo:voids" as="map(xs:string, xs:string+)">
    <xsl:map>
      <xsl:for-each select="unparsed-text-lines($voids)[tokenize(., '&#9;')[2] = $number]">
        <xsl:variable name="cells" select="tokenize(., '&#9;')"/>
        <xsl:map-entry key="$cells[1]" select="($cells[3], $cells[4])"/>
      </xsl:for-each>
    </xsl:map>
  </xsl:variable>
  <xsl:variable name="eo:doc" select="/"/>
  <!-- The sign a decision is spelled with, for every side a fork may take. -->
  <xsl:variable name="eo:signs" as="map(xs:string, xs:string)" select="map {'left': '+', 'right': '-'}"/>
  <!--
  The leaves of the tree of ifs: every path of the protocol, by the place
  it ends at, which is all of its decisions spelled one after another.
  -->
  <xsl:variable name="eo:leaves" as="map(xs:string, element())" select="map:merge(for $path in $eo:doc/protocol/path return map {eo:place(eo:decisions($path)): $path}, map {'duplicates': 'use-first'})"/>
  <!--
  The forks of the tree of ifs: for every place a path forked at, the
  condition it forked on there, as every path says it.
  -->
  <xsl:variable name="eo:forks" as="map(xs:string, xs:string)*">
    <xsl:for-each select="$eo:doc/protocol/path">
      <xsl:variable name="decisions" select="eo:decisions(.)"/>
      <xsl:for-each select="1 to count($decisions)">
        <xsl:variable name="index" select="."/>
        <xsl:sequence select="map {eo:place(subsequence($decisions, 1, $index - 1)): substring($decisions[$index], 1, string-length($decisions[$index]) - 1)}"/>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:variable>
  <!-- The forks of the tree of ifs, each place once. -->
  <xsl:variable name="eo:nodes" as="map(xs:string, xs:string)" select="map:merge($eo:forks, map {'duplicates': 'use-first'})"/>
  <xsl:template match="/">
    <rendered>
      <xsl:try>
        <xsl:sequence select="eo:checked()"/>
        <xsl:variable name="at" select="eo:placed()"/>
        <atom file="{eo:file()}" voids="{count(map:keys($at)[map:contains($eo:voids, .)])}" statements="{count(map:keys($at)[exists(key('eo:minted', ., $eo:doc)) or exists(key('eo:joined', ., $eo:doc)) or exists(key('eo:deferred', ., $eo:doc))])}" branches="{map:size($eo:nodes) + count(map:keys($at)[exists(key('eo:joined', ., $eo:doc))])}">
          <xsl:value-of select="eo:java(eo:block($at, (), 2))"/>
        </atom>
        <xsl:catch errors="eo:taint">
          <taint>
            <xsl:value-of select="$err:description"/>
          </taint>
        </xsl:catch>
      </xsl:try>
    </rendered>
  </xsl:template>
  <!-- Stop the rendering, saying why the entry is a taint. -->
  <xsl:function name="eo:taint">
    <xsl:param name="why" as="xs:string"/>
    <xsl:sequence select="error(QName('https://www.eolang.org', 'eo:taint'), $why)"/>
  </xsl:function>
  <!-- The Java package of the atom. -->
  <xsl:function name="eo:package" as="xs:string">
    <xsl:variable name="package" select="string-join(doc($source)/object/metas/meta[head = 'package']/tail/text(), '')"/>
    <xsl:sequence select="if ($package = '') then 'org.eolang' else concat('org.eolang.', eo:package-name($package))"/>
  </xsl:function>
  <!--
  The names from the top object down to the atom, which is the φ of the entry.
  The transpiler makes a class of its own of every formation with no name,
  which is an argument of some application, and names the atom inside it
  after the top object and the formations below that argument only: the atom
  of "Φ.true.φ.α0" is "true.φ", and the atom of a formation "inner" inside
  that argument is "true.inner.φ".
  -->
  <xsl:function name="eo:names" as="xs:string*">
    <xsl:variable name="formation" select="doc($source)//o[@loc = $locator][1]"/>
    <xsl:if test="empty($formation)">
      <xsl:sequence select="eo:taint(concat('The formation ', $locator, ' is not inside ', $top))"/>
    </xsl:if>
    <xsl:variable name="argument" select="$formation/ancestor-or-self::o[not(@name)][1]"/>
    <xsl:variable name="named" select="$formation/ancestor-or-self::o[empty($argument) or ancestor::o[. is $argument]]"/>
    <xsl:sequence select="(tokenize($top, '\.')[last()][exists($argument)], $named/@name ! string(.), 'φ')"/>
  </xsl:function>
  <!-- The simple name of the class of the atom. -->
  <xsl:function name="eo:class" as="xs:string">
    <xsl:sequence select="string-join(eo:names() ! eo:class-name(.), '$')"/>
  </xsl:function>
  <!-- The file of the atom, under the directory of generated sources. -->
  <xsl:function name="eo:file" as="xs:string">
    <xsl:sequence select="concat(replace(eo:package(), '\.', '/'), '/', eo:class(), '.java')"/>
  </xsl:function>
  <!-- The number of a path, counting from one in the order phino ran them. -->
  <xsl:function name="eo:nth" as="xs:integer">
    <xsl:param name="path" as="element()"/>
    <xsl:sequence select="count($path/preceding-sibling::path) + 1"/>
  </xsl:function>
  <!--
  The tree of the paths: first, the decisions of every path, by the number
  of the path, in the order the path came to them, which are the ones it
  took over from the path it left, with the fork that opened it turned to
  the right side, and then the ones it made itself; second, the number of
  the path every path left, by the number of the path, with zero for the
  first path, which left none. The paths are walked in the order phino
  wrote them, which is the order their forks opened them: every fork a
  path meets afresh, taking the left side, opens one more path, so the
  fork that opened a path is the first one, in that order, that no earlier
  path was opened by. A taint found on the way is kept as the reason, in
  place of the tree, since a try catches no error a global variable raises,
  and raised by "eo:tree" instead, inside the try of the rendering.
  -->
  <xsl:variable name="eo:tree" as="item()*">
    <xsl:try>
      <xsl:iterate select="$eo:doc/protocol/path">
        <xsl:param name="routes" as="map(xs:integer, xs:string*)" select="map {}"/>
        <xsl:param name="parents" as="map(xs:integer, xs:integer)" select="map {}"/>
        <xsl:param name="openers" as="xs:string*" select="()"/>
        <xsl:on-completion select="($routes, $parents)"/>
        <xsl:variable name="nth" select="position()"/>
        <xsl:variable name="opener" select="tokenize($openers[$nth - 1], ' ')"/>
        <xsl:variable name="inherited" as="xs:string*">
          <xsl:choose>
            <xsl:when test="$nth = 1"/>
            <xsl:when test="empty($opener)">
              <xsl:sequence select="eo:taint(concat('The path ', $nth, ' of the entry ', $number, ' was opened by no fork, while phino opens a path for every fork a path meets afresh'))"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:variable name="above" select="$routes(xs:integer($opener[1]))"/>
              <xsl:variable name="first" select="(.//forked)[1]"/>
              <xsl:if test="normalize-space($first) != $opener[2] or $first/@side != 'right'">
                <xsl:sequence select="eo:taint(concat('The path ', $nth, ' of the entry ', $number, ' opens with the side ', $first/@side, ' of ', normalize-space($first), ', while the fork that opened it, on the path ', $opener[1], ', is on ', $opener[2], ', whose right side it takes'))"/>
              </xsl:if>
              <xsl:sequence select="(subsequence($above, 1, index-of($above, concat($opener[2], '+')) - 1), concat($opener[2], '-'))"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:variable name="own" select="eo:made(., $inherited)"/>
        <xsl:next-iteration>
          <xsl:with-param name="routes" select="map:put($routes, $nth, ($inherited, $own))"/>
          <xsl:with-param name="parents" select="map:put($parents, $nth, xs:integer(($opener[1], 0)[1]))"/>
          <xsl:with-param name="openers" select="($openers, $own[ends-with(., '+')] ! concat($nth, ' ', substring(., 1, string-length(.) - 1)))"/>
        </xsl:next-iteration>
      </xsl:iterate>
      <xsl:catch errors="eo:taint" select="$err:description"/>
    </xsl:try>
  </xsl:variable>
  <!-- The tree of the paths, or the taint found while building it. -->
  <xsl:function name="eo:tree" as="map(xs:integer, item()*)*">
    <xsl:sequence select="if ($eo:tree instance of xs:string) then eo:taint($eo:tree) else $eo:tree"/>
  </xsl:function>
  <!--
  The decisions a path made itself, in the order it made them: the
  condition of every fork it met afresh, with "+" when it took the left
  side and "-" when it took the right one. A fork on a condition the path
  decided before, or took over from the path it left, takes the side
  decided then, so one condition is one decision, whatever the number of
  forks on it.
  -->
  <xsl:function name="eo:made" as="xs:string*">
    <xsl:param name="path" as="element()"/>
    <xsl:param name="inherited" as="xs:string*"/>
    <xsl:for-each-group select="$path//forked" group-by="normalize-space(.)">
      <xsl:variable name="sides" select="distinct-values(current-group()/@side)"/>
      <xsl:if test="count($sides) != 1 or not(map:contains($eo:signs, $sides[1]))">
        <xsl:sequence select="eo:taint(concat('The path ', eo:nth($path), ' of the entry ', $number, ' takes the side ', string-join($sides, ' and then '), ' of ', current-grouping-key(), ', while a path takes one side of a condition, left or right'))"/>
      </xsl:if>
      <xsl:variable name="decision" select="concat(current-grouping-key(), $eo:signs($sides[1]))"/>
      <xsl:variable name="before" select="$inherited[substring(., 1, string-length(.) - 1) = current-grouping-key()]"/>
      <xsl:choose>
        <xsl:when test="empty($before)">
          <xsl:sequence select="$decision"/>
        </xsl:when>
        <xsl:when test="$before != $decision">
          <xsl:sequence select="eo:taint(concat('The path ', eo:nth($path), ' of the entry ', $number, ' takes the side ', $sides[1], ' of ', current-grouping-key(), ', while the path it left took the other one there'))"/>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each-group>
  </xsl:function>
  <!-- The decisions of a path, the ones it took over and the ones it made. -->
  <xsl:function name="eo:decisions" as="xs:string*">
    <xsl:param name="path" as="element()"/>
    <xsl:sequence select="eo:tree()[1](eo:nth($path))"/>
  </xsl:function>
  <!-- The path a path left at the fork that opened it, or nothing for the first path. -->
  <xsl:function name="eo:parent" as="element()?">
    <xsl:param name="path" as="element()"/>
    <xsl:sequence select="$eo:doc/protocol/path[eo:tree()[2](eo:nth($path))]"/>
  </xsl:function>
  <!-- A place in the tree of ifs, which is a list of decisions, as one string. -->
  <xsl:function name="eo:place" as="xs:string">
    <xsl:param name="decisions" as="xs:string*"/>
    <xsl:sequence select="string-join($decisions, ' ')"/>
  </xsl:function>
  <!-- The decisions of a place spelled as one string. -->
  <xsl:function name="eo:tokens" as="xs:string*">
    <xsl:param name="place" as="xs:string"/>
    <xsl:sequence select="tokenize($place, ' ')[. != '']"/>
  </xsl:function>
  <!-- The words that name a place in a taint. -->
  <xsl:function name="eo:at" as="xs:string">
    <xsl:param name="place" as="xs:string"/>
    <xsl:sequence select="if ($place = '') then 'before any decision' else concat('after ', $place)"/>
  </xsl:function>
  <!-- The words that name a path in a taint. -->
  <xsl:function name="eo:route" as="xs:string">
    <xsl:param name="path" as="element()"/>
    <xsl:variable name="decisions" select="eo:decisions($path)"/>
    <xsl:sequence select="concat('the path ', eo:nth($path), if (exists($decisions)) then concat(', which takes ', string-join($decisions, ' ')) else '')"/>
  </xsl:function>
  <!--
  Stop the rendering of an entry whose protocol is no tree of ifs: one that
  ran out of time, holds no path, defines a symbol twice, has two paths
  that made the same decisions, forks on two conditions at one place, ends
  a path where another one forks, or forks on a side phino never ran, as
  it does when "max-paths" cuts the run.
  -->
  <xsl:function name="eo:checked">
    <xsl:variable name="timeout" select="($eo:doc//timeout)[1]"/>
    <xsl:if test="exists($timeout)">
      <xsl:sequence select="eo:taint(concat('The entry ', $number, ' ran out of ', $timeout/@limit, ' seconds at ', $timeout/@at))"/>
    </xsl:if>
    <xsl:if test="empty($eo:doc/protocol/path)">
      <xsl:sequence select="eo:taint(concat('The protocol of the entry ', $number, ' holds no path, while phino writes one for every run'))"/>
    </xsl:if>
    <xsl:for-each-group select="$eo:doc//(minted | joined | symbolized | deferred)/@symbol" group-by=".">
      <xsl:if test="count(current-group()) &gt; 1">
        <xsl:sequence select="eo:taint(concat('The symbol ', current-grouping-key(), ' of the entry ', $number, ' is defined ', count(current-group()), ' times, while phino mints every symbol once'))"/>
      </xsl:if>
    </xsl:for-each-group>
    <xsl:for-each-group select="$eo:doc/protocol/path" group-by="eo:place(eo:decisions(.))">
      <xsl:if test="count(current-group()) &gt; 1">
        <xsl:sequence select="eo:taint(concat('The paths ', string-join(current-group() ! string(eo:nth(.)), ' and '), ' of the entry ', $number, ' all end ', eo:at(current-grouping-key()), ', while every path makes decisions of its own'))"/>
      </xsl:if>
    </xsl:for-each-group>
    <xsl:for-each-group select="$eo:forks" group-by="map:keys(.)">
      <xsl:variable name="conditions" select="distinct-values(current-group() ! map:get(., current-grouping-key()))"/>
      <xsl:if test="count($conditions) &gt; 1">
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' forks on ', string-join($conditions, ' and on '), ' ', eo:at(current-grouping-key()), ', while every run forks on one condition there'))"/>
      </xsl:if>
    </xsl:for-each-group>
    <xsl:for-each select="map:keys($eo:nodes)">
      <xsl:variable name="place" select="."/>
      <xsl:variable name="condition" select="$eo:nodes($place)"/>
      <xsl:if test="map:contains($eo:leaves, $place)">
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' ends ', eo:route($eo:leaves($place)), ' ', eo:at($place), ', where another path forks on ', $condition))"/>
      </xsl:if>
      <xsl:for-each select="('+', '-')[not(map:contains($eo:leaves, eo:place((eo:tokens($place), concat($condition, .))))) and not(map:contains($eo:nodes, eo:place((eo:tokens($place), concat($condition, .)))))][1]">
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' forks on ', $condition, ' ', eo:at($place), ', while phino never ran the path that takes ', $condition, ., ', as it does when the paths are cut'))"/>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:function>
  <!--
  Stop the rendering of an entry with a path that dataized no root, saying
  what the protocol shows instead: a body that was ⊥ from the start, the
  last step phino got no answer for, the loop phino cut, or the last λ phino
  got stuck at.
  -->
  <xsl:function name="eo:rootless">
    <xsl:param name="path" as="element()"/>
    <xsl:variable name="unanswered" select="string(($path//unanswered)[last()])"/>
    <xsl:variable name="looped" select="($path//looped)[last()]"/>
    <xsl:variable name="prefix" select="concat('The entry ', $number, ' came to no root on ', eo:route($path), ', because ')"/>
    <xsl:choose>
      <xsl:when test="empty($path/morph/evaluate[@λ = 'L_root']) and $path//evaluate[@λ = 'L_entry']/bound[@meta = '𝑛1·1'] = '⊥'">
        <xsl:sequence select="eo:taint(concat('The body of the entry ', $number, ' reduced to ⊥ before phino computed anything, so its root was never dataized'))"/>
      </xsl:when>
      <xsl:when test="$unanswered = '⊥'">
        <xsl:sequence select="eo:taint(concat($prefix, 'phino came to a step that reduced to ⊥ and could not go on'))"/>
      </xsl:when>
      <xsl:when test="$unanswered != ''">
        <xsl:sequence select="eo:taint(concat($prefix, 'phino got no answer for ', $unanswered))"/>
      </xsl:when>
      <xsl:when test="exists($looped)">
        <xsl:sequence select="eo:taint(concat($prefix, 'phino cut a loop at ', $looped/@of, ' and got stuck at ', (($path//stuck[@λ])[last()]/@λ, 'nothing')[1]))"/>
      </xsl:when>
      <xsl:when test="exists($path//stuck[@λ])">
        <xsl:sequence select="eo:taint(concat($prefix, 'phino got stuck at ', ($path//stuck[@λ])[last()]/@λ))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:taint(concat($prefix, 'its protocol says nothing about why'))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- The record of the copy of "throw" a path stopped at right where it dataized its root, or nothing. -->
  <xsl:function name="eo:stop" as="element()?">
    <xsl:param name="path" as="element()"/>
    <xsl:sequence select="($path/morph/evaluate[@λ = 'L_root']/dataize/unanswered[@λ = 'L_throw'])[1]"/>
  </xsl:function>
  <!--
  The message a path throws, as the body of a Java string literal, or
  nothing when the path does not throw. The path throws when phino stopped
  at the λ of "throw" right where it dataized the root, and then the
  protocol names the copy of "throw" it stopped at, and the record that
  applied "throw" to its message. The message is a string literal when
  "eo:octets" finds its bytes, and those bytes are text in UTF-8. A message
  that is anything else is an object the atom would have to compute, and an
  entry that throws it is a taint, as is an entry whose protocol does not
  say what the message is.
  -->
  <xsl:function name="eo:thrown" as="xs:string?">
    <xsl:param name="path" as="element()"/>
    <xsl:variable name="stop" select="eo:stop($path)"/>
    <xsl:if test="exists($stop)">
      <xsl:variable name="message" select="normalize-space(($eo:doc//(applied | formed)[@meta = normalize-space($stop)]/*[@name = 'message'])[1])"/>
      <xsl:variable name="bytes" select="eo:octets($message)"/>
      <xsl:variable name="prefix" select="concat('The entry ', $number, ' throws on ', eo:route($path), ', but ')"/>
      <xsl:choose>
        <xsl:when test="$message = ''">
          <xsl:sequence select="eo:taint(concat($prefix, 'phino stopped at L_throw for ', normalize-space($stop), ', while no record of its protocol gives the message'))"/>
        </xsl:when>
        <xsl:when test="empty($bytes)">
          <xsl:sequence select="eo:taint(concat($prefix, 'its message ', $message, ' is not a string literal, and an atom that computes its message is not written yet'))"/>
        </xsl:when>
        <xsl:when test="eo:text($bytes) = -1">
          <xsl:sequence select="eo:taint(concat($prefix, 'its message ', $bytes, ' is no text in UTF-8'))"/>
        </xsl:when>
      </xsl:choose>
      <xsl:sequence select="eo:message($bytes)"/>
    </xsl:if>
  </xsl:function>
  <!--
  The bytes of a message the protocol names by the meta of its record, as
  phino writes bytes, or nothing when the message is not the record that
  applies "Φ.string" to the record of literal bytes alone, which is how
  phino writes a string literal.
  -->
  <xsl:function name="eo:octets" as="xs:string?">
    <xsl:param name="message" as="xs:string"/>
    <xsl:variable name="string" select="($eo:doc//(applied | formed)[@meta = $message])[1][@of = 'Φ.string' and count(*) = 1]/meta[@name = 'φ']"/>
    <xsl:variable name="bytes" select="normalize-space(($eo:doc//(applied | formed)[@meta = normalize-space($string)])[1][@of = 'Φ.bytes' and count(*) = 1]/datum[@name = 'φ'])"/>
    <xsl:if test="matches($bytes, '^(--|[0-9A-F]{2}-|[0-9A-F]{2}(-[0-9A-F]{2})+)$')">
      <xsl:sequence select="$bytes"/>
    </xsl:if>
  </xsl:function>
  <!-- The code points of bytes read as text in UTF-8, with -1 where they are no such text. -->
  <xsl:function name="eo:text" as="xs:integer*">
    <xsl:param name="bytes" as="xs:string"/>
    <xsl:sequence select="eo:utf8(tokenize($bytes, '-')[. != ''] ! fold-left(string-to-codepoints(upper-case(.)), 0, function($n, $c) { $n * 16 + string-length(substring-before('0123456789ABCDEF', codepoints-to-string($c))) }))"/>
  </xsl:function>
  <!--
  The code points of octets in UTF-8, ending with -1 where the octets stop
  being such text: an octet that starts no character, a character cut short
  or written longer than it needs, a surrogate, or a code point past the
  last one.
  -->
  <xsl:function name="eo:utf8" as="xs:integer*">
    <xsl:param name="octets" as="xs:integer*"/>
    <xsl:variable name="lead" select="head($octets)"/>
    <xsl:variable name="size" select="if (empty($lead)) then 0 else if ($lead lt 128) then 1 else if ($lead lt 192) then -1 else if ($lead lt 224) then 2 else if ($lead lt 240) then 3 else if ($lead lt 248) then 4 else -1"/>
    <xsl:choose>
      <xsl:when test="$size = 0"/>
      <xsl:when test="$size = -1">
        <xsl:sequence select="-1"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="rest" select="subsequence($octets, 2, $size - 1)"/>
        <xsl:variable name="code" select="fold-left($rest, $lead mod (128, 32, 16, 8)[$size], function($c, $o) { $c * 64 + $o mod 64 })"/>
        <xsl:choose>
          <xsl:when test="count($rest) lt $size - 1 or (some $o in $rest satisfies $o idiv 64 != 2) or $code lt (0, 128, 2048, 65536)[$size] or $code gt 1114111 or ($code ge 55296 and $code le 57343)">
            <xsl:sequence select="-1"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:sequence select="($code, eo:utf8(subsequence($octets, $size + 1)))"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The escapes of the characters a Java string literal cannot hold as they
  are: a quote, a backslash, the ends of a line, a tab, and the two
  characters XML cannot hold, which Java reads as escapes before anything
  else, where they do no harm.
  -->
  <xsl:variable name="eo:escapes" as="map(xs:integer, xs:string)" select="map {9: '\t', 10: '\n', 13: '\r', 34: '\&quot;', 92: '\\', 65534: '\uFFFE', 65535: '\uFFFF'}"/>
  <!--
  The bytes of a message, as the body of a Java string literal. Any other
  control character becomes an octal escape of three digits, since Java
  reads a "\u" escape before it splits the source into lines.
  -->
  <xsl:function name="eo:message" as="xs:string">
    <xsl:param name="bytes" as="xs:string"/>
    <xsl:sequence select="string-join(eo:text($bytes) ! (if (map:contains($eo:escapes, .)) then $eo:escapes(.) else if (. lt 32 or . = 127) then concat('\', . idiv 64, . idiv 8 mod 8, . mod 8) else codepoints-to-string(.)), '')"/>
  </xsl:function>
  <!--
  The symbol the root of a path dataizes, or nothing when the path throws
  or came down to a constant. A path that did none of these is a taint.
  -->
  <xsl:function name="eo:root" as="xs:string?">
    <xsl:param name="path" as="element()"/>
    <xsl:variable name="root" select="eo:rooted($path)/manufactured"/>
    <xsl:choose>
      <xsl:when test="exists($root)">
        <xsl:variable name="symbol" select="string($root)"/>
        <xsl:if test="not(eo:constant($symbol)) and ((map:contains($eo:voids, $symbol) and exists(eo:returns($locator))) or (not(map:contains($eo:voids, $symbol)) and not(eo:type($symbol) = 'Phi'))) and empty(eo:around($path))">
          <xsl:sequence select="eo:bare($symbol)"/>
        </xsl:if>
        <xsl:sequence select="$symbol"/>
      </xsl:when>
      <xsl:when test="exists(eo:stop($path)) or exists(eo:fixed($path))"/>
      <xsl:otherwise>
        <xsl:sequence select="eo:rootless($path)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The record of the dataization of the root of a path, which is the one
  the λ of the root binds to its first meta right after, or nothing when
  the path stopped inside it. The record ends in the symbol the root came
  down to, or in its bytes.
  -->
  <xsl:function name="eo:rooted" as="element()?">
    <xsl:param name="path" as="element()"/>
    <xsl:sequence select="($path/morph/evaluate[@λ = 'L_root']/dataize[following-sibling::*[1][self::bound][starts-with(@meta, '𝛿1·')]])[last()]"/>
  </xsl:function>
  <!--
  The bytes a path came down to right where it dataized its root, or
  nothing when it dataized a symbol there, or stopped.
  -->
  <xsl:function name="eo:fixed" as="xs:string?">
    <xsl:param name="path" as="element()"/>
    <xsl:variable name="bytes" select="normalize-space(eo:rooted($path)/dataized)"/>
    <xsl:if test="$bytes != ''">
      <xsl:sequence select="$bytes"/>
    </xsl:if>
  </xsl:function>
  <!--
  Stop the rendering of a path whose root is data the atom would return
  bare, while the protocol does not name the object around it, unless the
  tables of "eo:inference" say that the body is a number, a bool or bytes,
  which is what "Data.ToPhi" makes of the data anyway. Bare data loses the
  object around it, so the atom of a body of any other type, a string or
  an "i16" for example, would lose every attribute of its own. A body the
  tables say nothing about may be anything, so its entry is a taint as
  well.
  -->
  <xsl:function name="eo:bare">
    <xsl:param name="root" as="xs:string"/>
    <xsl:variable name="types" as="xs:string*" select="eo:returns($locator)"/>
    <xsl:variable name="others" as="xs:string*" select="$types[not(. = ('Φ.number', 'Φ.bool', 'Φ.true', 'Φ.false', 'Φ.bytes'))]"/>
    <xsl:choose>
      <xsl:when test="empty($types)">
        <xsl:sequence select="eo:taint(concat('The root ', $root, ' of the entry ', $number, ' is bare data, while inference does not say what ', $locator, ' returns'))"/>
      </xsl:when>
      <xsl:when test="exists($others)">
        <xsl:sequence select="eo:taint(concat('The root ', $root, ' of the entry ', $number, ' is bare data, while inference says ', $locator, ' returns ', string-join($others, ' and '), ', and bare data would lose the object around it'))"/>
      </xsl:when>
    </xsl:choose>
  </xsl:function>
  <!--
  The Java type of the data a path came down to, as the tables of
  "eo:inference" say what the body returns, which is the only place that
  says what bytes alone are: a bool, a number, or bytes.
  -->
  <xsl:function name="eo:carrier" as="xs:string">
    <xsl:param name="bytes" as="xs:string"/>
    <xsl:variable name="types" as="xs:string*" select="eo:returns($locator)"/>
    <xsl:sequence select="eo:bare($bytes)"/>
    <xsl:choose>
      <xsl:when test="every $t in $types satisfies $t = ('Φ.bool', 'Φ.true', 'Φ.false')">
        <xsl:sequence select="'boolean'"/>
      </xsl:when>
      <xsl:when test="every $t in $types satisfies $t = 'Φ.number'">
        <xsl:sequence select="'double'"/>
      </xsl:when>
      <xsl:when test="every $t in $types satisfies $t = 'Φ.bytes'">
        <xsl:sequence select="'byte[]'"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:taint(concat('The root ', $bytes, ' of the entry ', $number, ' is bare data, while inference says ', $locator, ' returns ', string-join($types, ' or '), ', and bare data is one of them only'))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- The object a void is, as a chain of takes off the atom. -->
  <xsl:function name="eo:object" as="xs:string">
    <xsl:param name="void" as="xs:string"/>
    <xsl:sequence select="string-join(('this.take(&quot;ρ&quot;)', tokenize($eo:voids($void)[1], '\.') ! concat('.take(&quot;', eo:literal(.), '&quot;)')), '')"/>
  </xsl:function>
  <!--
  The object the body puts around its data, which the protocol names in
  the answer of "L_entry" for the body, or nothing: an object of the world,
  outside the entry, applied to its "φ" alone. Such an object dataizes
  through its "φ", so the data of the root is the data of that "φ", and the
  atom gives the object back by applying it to the root again. An answer
  that names anything else, an object with more arguments, one inside the
  entry, the carrier of a bool, or the application of a dispatch, names no
  such object, and then the root is returned the way its own type tells.
  The answer stands in the path that bound it, or in the path that path
  left, when the fork that opened it came after the answer, since a path
  holds only what happened after that fork.
  -->
  <xsl:function name="eo:around" as="xs:string?">
    <xsl:param name="path" as="element()"/>
    <xsl:variable name="answer" select="normalize-space(($path//evaluate[@λ = 'L_entry']/bound[@meta = '𝑛1·1'])[1])"/>
    <xsl:variable name="parent" select="eo:parent($path)"/>
    <xsl:choose>
      <xsl:when test="$answer = '' and exists($parent)">
        <xsl:sequence select="eo:around($parent)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="applied" select="($eo:doc//(applied | formed)[@meta = $answer])[1]"/>
        <xsl:if test="exists($applied) and count($applied/*) = 1 and $applied/*/@name = 'φ' and matches($applied/@of, '^Φ(\.[^.\s()]+)+$') and not(starts-with($applied/@of, concat($locator, '.')))">
          <xsl:sequence select="string($applied/@of)"/>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- The objects "Data.ToPhi" makes of a Java value, each with the type of that value. -->
  <xsl:variable name="eo:carriers" as="map(xs:string, xs:string)" select="map {'Φ.number': 'double', 'Φ.bytes': 'byte[]'}"/>
  <!--
  The Java of the root of a path as the object the atom returns: inside the
  object the protocol names around it, or, when the protocol names none, a
  void and a copy as they are, and data wrapped into the carrier of its type.
  -->
  <xsl:function name="eo:returned" as="xs:string">
    <xsl:param name="root" as="xs:string"/>
    <xsl:param name="path" as="element()"/>
    <xsl:variable name="around" select="eo:around($path)"/>
    <xsl:choose>
      <xsl:when test="empty($around)">
        <xsl:sequence select="eo:argument($root)"/>
      </xsl:when>
      <xsl:when test="map:contains($eo:carriers, $around) and not(map:contains($eo:voids, $root)) and not(eo:type($root) = 'Phi')">
        <xsl:sequence select="concat('new Data.ToPhi(', eo:value($root, $eo:carriers($around)), ')')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="concat('new PhApplication(', eo:global($around), ', &quot;φ&quot;, ', eo:argument($root), ')')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The Java of the bytes a path came down to as the object the atom returns:
  inside the object the protocol names around them, or, when the protocol
  names none, wrapped into the carrier the tables of "eo:inference" give
  the body.
  -->
  <xsl:function name="eo:constant-returned" as="xs:string">
    <xsl:param name="bytes" as="xs:string"/>
    <xsl:param name="path" as="element()"/>
    <xsl:variable name="around" select="eo:around($path)"/>
    <xsl:choose>
      <xsl:when test="empty($around)">
        <xsl:sequence select="concat('new Data.ToPhi(', eo:literal-of($bytes, eo:carrier($bytes)), ')')"/>
      </xsl:when>
      <xsl:when test="map:contains($eo:carriers, $around)">
        <xsl:sequence select="concat('new Data.ToPhi(', eo:literal-of($bytes, $eo:carriers($around)), ')')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="concat('new PhApplication(', eo:global($around), ', &quot;φ&quot;, ', eo:literal-of($bytes, 'Phi'), ')')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- An object of the world, as a chain of takes off the root of the universe. -->
  <xsl:function name="eo:global" as="xs:string">
    <xsl:param name="object" as="xs:string"/>
    <xsl:sequence select="string-join(('Phi.Φ', tokenize(substring-after($object, 'Φ.'), '\.') ! concat('take(&quot;', eo:literal(.), '&quot;)')), '.')"/>
  </xsl:function>
  <!--
  The last statement of a leaf of the tree of ifs: the return of what the
  path came to, or the throw of the message the path stopped at.
  -->
  <xsl:function name="eo:ending" as="xs:string">
    <xsl:param name="path" as="element()"/>
    <xsl:param name="depth" as="xs:integer"/>
    <xsl:variable name="indent" select="string-join((1 to $depth) ! '    ', '')"/>
    <xsl:variable name="thrown" select="eo:thrown($path)"/>
    <xsl:variable name="root" select="eo:root($path)"/>
    <xsl:choose>
      <xsl:when test="exists($thrown)">
        <xsl:sequence select="concat($indent, 'throw new ExFailure(&quot;%s&quot;, &quot;', $thrown, '&quot;);&#10;')"/>
      </xsl:when>
      <xsl:when test="exists($root) and eo:constant($root)">
        <xsl:sequence select="concat($indent, 'return ', eo:constant-returned(eo:bytes($root), $path), ';&#10;')"/>
      </xsl:when>
      <xsl:when test="exists($root)">
        <xsl:sequence select="concat($indent, 'return ', eo:returned($root, $path), ';&#10;')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="concat($indent, 'return ', eo:constant-returned(eo:fixed($path), $path), ';&#10;')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- The whole Java file of the atom, around the body of its lambda. -->
  <xsl:function name="eo:java" as="xs:string">
    <xsl:param name="body" as="xs:string"/>
    <xsl:variable name="class" select="eo:class()"/>
    <xsl:sequence select="string-join(('/*', ' * This file was generated by eo-lowering, from the protocol of the entry', concat(' * ', $number, ', which is ', $locator, '.'), ' */', concat('package ', eo:package(), ';'), '', 'import org.eolang.*;', '', '/**', concat(' * The atom that took the place of the body of ', $locator, '.'), ' */', concat('@XmirObject(oname = &quot;', eo:literal(string-join(eo:names(), '.')), '&quot;)'), concat('public final class ', $class, ' extends PhDefault implements Atom {'), concat('    public ', $class, '() {'), '        super(new Attrs(new Attr(Phi.RHO, new AtRho())));', '    }', '', '    @Override', '    public Phi lambda() {', concat($body, '    }'), '}', ''), '&#10;')"/>
  </xsl:function>
  <!--
  Where every symbol some path reaches is placed, as a map from the symbol
  to its place: the decisions of the ifs it sits inside, from the outermost
  in. The roots are read at the leaves, and the condition of every fork at
  the place of that fork. The symbols are walked from the highest number
  down, and each one tells the symbols it reads where it reads them, so
  that a symbol read in two places is placed where both places agree. A
  root that is a void is returned as the object it is, and is never read as
  data at all, and neither is a root that is a constant. A joined symbol
  reads its condition where it sits, and each of its branches inside the
  side of its own if, which is the place of the joined symbol followed by
  the sign of that side and the symbol.
  -->
  <xsl:function name="eo:placed" as="map(xs:string, xs:string*)">
    <xsl:variable name="all" select="distinct-values(($eo:doc//(minted | joined | deferred)/@symbol, map:keys($eo:voids)))"/>
    <xsl:variable name="merged" as="function(*)" select="function($m, $r) { let $k := map:keys($r) return map:put($m, $k, if (map:contains($m, $k)) then eo:common($m($k), $r($k)) else $r($k)) }"/>
    <xsl:variable name="seeds" as="map(xs:string, xs:string*)*" select="(for $place in map:keys($eo:leaves) return (let $root := eo:root($eo:leaves($place)) return if (exists($root) and not(map:contains($eo:voids, $root)) and not(eo:constant($root))) then map {$root: eo:tokens($place)} else ()), for $place in map:keys($eo:nodes) return (if (eo:constant($eo:nodes($place))) then () else map {$eo:nodes($place): eo:tokens($place)}))"/>
    <xsl:iterate select="sort($all, (), function($s) { -eo:serial($s) })">
      <xsl:param name="at" as="map(xs:string, xs:string*)" select="fold-left($seeds, map {}, $merged)"/>
      <xsl:on-completion>
        <xsl:for-each select="map:keys($at)[not(. = $all)][1]">
          <xsl:sequence select="eo:taint(concat('The symbol ', ., ' of the entry ', $number, ' is read, while nothing defines it'))"/>
        </xsl:for-each>
        <xsl:sequence select="$at"/>
      </xsl:on-completion>
      <xsl:variable name="symbol" select="."/>
      <xsl:variable name="here" select="$at($symbol)"/>
      <xsl:variable name="reads" as="map(xs:string, xs:string*)*">
        <xsl:choose>
          <xsl:when test="not(map:contains($at, $symbol))"/>
          <xsl:when test="exists(key('eo:joined', $symbol, $eo:doc))">
            <xsl:variable name="joined" select="key('eo:joined', $symbol, $eo:doc)[1]"/>
            <xsl:sequence select="(map {eo:condition($joined): $here}, map {eo:branch($joined, 'left'): ($here, concat('+', $symbol))}, map {eo:branch($joined, 'right'): ($here, concat('-', $symbol))})"/>
          </xsl:when>
          <xsl:when test="exists(key('eo:deferred', $symbol, $eo:doc))">
            <xsl:sequence select="eo:arguments(key('eo:deferred', $symbol, $eo:doc)[1])[not(map:contains($eo:voids, substring-after(., ':')))] ! map {substring-after(., ':'): $here}"/>
          </xsl:when>
          <xsl:when test="map:contains($eo:voids, $symbol)"/>
          <xsl:otherwise>
            <xsl:sequence select="eo:operands($symbol) ! map {.: $here}"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="symbols" select="$reads[not(eo:constant(map:keys(.)))]"/>
      <xsl:for-each select="$symbols[eo:serial(map:keys(.)) &gt;= eo:serial($symbol)][1]">
        <xsl:sequence select="eo:taint(concat('The symbol ', $symbol, ' of the entry ', $number, ' reads ', map:keys(.), ', which was not minted before it'))"/>
      </xsl:for-each>
      <xsl:next-iteration>
        <xsl:with-param name="at" select="fold-left($symbols, $at, $merged)"/>
      </xsl:next-iteration>
    </xsl:iterate>
  </xsl:function>
  <!-- The places two readers read a symbol at have this place in common. -->
  <xsl:function name="eo:common" as="xs:string*">
    <xsl:param name="left" as="xs:string*"/>
    <xsl:param name="right" as="xs:string*"/>
    <xsl:sequence select="if (exists($left) and exists($right) and $left[1] = $right[1]) then ($left[1], eo:common(tail($left), tail($right))) else ()"/>
  </xsl:function>
  <!-- The number of a symbol, which is the order phino minted it in. -->
  <xsl:function name="eo:serial" as="xs:integer">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:if test="not(matches($symbol, '^𝜎\d+$'))">
      <xsl:sequence select="eo:taint(concat('The symbol ', $symbol, ' of the entry ', $number, ' has no number'))"/>
    </xsl:if>
    <xsl:sequence select="xs:integer(substring($symbol, 2))"/>
  </xsl:function>
  <!-- The Java local a symbol is held in. -->
  <xsl:function name="eo:local" as="xs:string">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:sequence select="concat('s', substring($symbol, 2))"/>
  </xsl:function>
  <!-- A token is a constant when it is bytes written out, or a symbolized literal. -->
  <xsl:function name="eo:constant" as="xs:boolean">
    <xsl:param name="token" as="xs:string"/>
    <xsl:sequence select="not(starts-with($token, '𝜎')) or exists(key('eo:symbolized', $token, $eo:doc))"/>
  </xsl:function>
  <!-- The bytes of a constant token. -->
  <xsl:function name="eo:bytes" as="xs:string">
    <xsl:param name="token" as="xs:string"/>
    <xsl:sequence select="if (starts-with($token, '𝜎')) then normalize-space(key('eo:symbolized', $token, $eo:doc)[1]) else $token"/>
  </xsl:function>
  <!-- The condition a joined symbol depends on. -->
  <xsl:function name="eo:condition" as="xs:string">
    <xsl:param name="joined" as="element()"/>
    <xsl:sequence select="normalize-space($joined)"/>
  </xsl:function>
  <!-- The symbol a joined symbol stands for on one side, left or right. -->
  <xsl:function name="eo:branch" as="xs:string">
    <xsl:param name="joined" as="element()"/>
    <xsl:param name="side" as="xs:string"/>
    <xsl:sequence select="if ($side = 'left') then string($joined/@left) else string($joined/@right)"/>
  </xsl:function>
  <!-- The symbols a minted symbol was made of, in the order of its operands. -->
  <xsl:function name="eo:operands" as="xs:string*">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:sequence select="tokenize(normalize-space(key('eo:minted', $symbol, $eo:doc)[1]), ' ')"/>
  </xsl:function>
  <!--
  The arguments a deferred copy takes, each as its name and its symbol with
  a colon between them. A copy with an argument that is no symbol cannot be
  made, since the atom has nothing to bind that argument to, and a void left
  unset fails the copy once the program runs.
  -->
  <xsl:function name="eo:arguments" as="xs:string*">
    <xsl:param name="deferred" as="element()"/>
    <xsl:for-each select="$deferred/with/*[not(self::symbol)][1]">
      <xsl:sequence select="eo:taint(concat('The deferred symbol ', $deferred/@symbol, ' of the entry ', $number, ' has the argument ', @name, ', which is no symbol'))"/>
    </xsl:for-each>
    <xsl:sequence select="$deferred/with/symbol ! concat(@name, ':', normalize-space(.))"/>
  </xsl:function>
  <!--
  The object a deferred copy is made of, as a chain of takes off the atom.
  It must be inside the formation of the entry, since the atom reaches
  nothing else, and outside its "φ", since the patching drops that "φ" along
  with all it holds. A copy of a formation no object of the world declares,
  which phino names by the record that spells the formation out, is one
  the atom cannot reach either.
  -->
  <xsl:function name="eo:copied" as="xs:string">
    <xsl:param name="deferred" as="element()"/>
    <xsl:variable name="of" select="string($deferred/@of)"/>
    <xsl:variable name="names" select="tokenize(substring-after($of, concat($locator, '.')), '\.')"/>
    <xsl:choose>
      <xsl:when test="starts-with($of, '𝑛')">
        <xsl:sequence select="eo:taint(concat('The deferred symbol ', $deferred/@symbol, ' of the entry ', $number, ' copies the formation ', $of, ', which no object of the world declares'))"/>
      </xsl:when>
      <xsl:when test="empty($names)">
        <xsl:sequence select="eo:taint(concat('The deferred symbol ', $deferred/@symbol, ' of the entry ', $number, ' copies ', $of, ', which is not inside ', $locator))"/>
      </xsl:when>
      <xsl:when test="$names[1] = 'φ'">
        <xsl:sequence select="eo:taint(concat('The deferred symbol ', $deferred/@symbol, ' of the entry ', $number, ' copies ', $of, ', which the atom replaces'))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="string-join(('this.take(&quot;ρ&quot;)', $names ! concat('.take(&quot;', eo:literal(.), '&quot;)')), '')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The Java of a symbol as an object a copy takes: a void and a copy as they
  are, anything else wrapped back into an object.
  -->
  <xsl:function name="eo:argument" as="xs:string">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:sequence select="if (map:contains($eo:voids, $symbol)) then eo:object($symbol) else eo:value($symbol, 'Phi')"/>
  </xsl:function>
  <!-- The operation a minted symbol was minted by. -->
  <xsl:function name="eo:operation" as="element()">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:variable name="lambda" select="string(key('eo:minted', $symbol, $eo:doc)[1]/../@λ)"/>
    <xsl:variable name="op" select="$eo:operations[@λ = $lambda]"/>
    <xsl:if test="empty($op)">
      <xsl:sequence select="eo:taint(concat('The symbol ', $symbol, ' of the entry ', $number, ' is minted by ', $lambda, ', which has no Java'))"/>
    </xsl:if>
    <xsl:sequence select="$op"/>
  </xsl:function>
  <!--
  The Java type of a symbol. A void that holds a number is bytes, like a
  void of any carrier but a bool, since a number may hold any bytes and a
  double holds exactly eight. The bytes are read as a double only where an
  operation wants one, which is where EO reads them as a number too. A
  copy is an object, and so is a joined symbol with a copy in one of its
  branches, since it holds the object of the branch taken; a joined symbol
  of two constants is a bool when both are the bytes of one, and bytes
  otherwise.
  -->
  <xsl:function name="eo:type" as="xs:string">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:variable name="joined" select="key('eo:joined', $symbol, $eo:doc)[1]"/>
    <xsl:choose>
      <xsl:when test="map:contains($eo:voids, $symbol)">
        <xsl:sequence select="(map {'bool': 'boolean'}($eo:voids($symbol)[2]), 'byte[]')[1]"/>
      </xsl:when>
      <xsl:when test="exists(key('eo:deferred', $symbol, $eo:doc))">
        <xsl:sequence select="'Phi'"/>
      </xsl:when>
      <xsl:when test="exists($joined)">
        <xsl:variable name="branches" select="(eo:branch($joined, 'left'), eo:branch($joined, 'right'))"/>
        <xsl:variable name="types" select="distinct-values($branches[not(eo:constant(.))] ! eo:type(.))"/>
        <xsl:choose>
          <xsl:when test="$types = 'Phi'">
            <xsl:sequence select="'Phi'"/>
          </xsl:when>
          <xsl:when test="count($types) = 1">
            <xsl:sequence select="$types"/>
          </xsl:when>
          <xsl:when test="count($types) &gt; 1">
            <xsl:sequence select="eo:taint(concat('The branches of the joined symbol ', $symbol, ' of the entry ', $number, ' are ', string-join($types, ' and ')))"/>
          </xsl:when>
          <xsl:when test="every $b in $branches satisfies upper-case(eo:bytes($b)) = ('FF-', '00-')">
            <xsl:sequence select="'boolean'"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:sequence select="'byte[]'"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="string(eo:operation($symbol)/@type)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- The Java of a token, as a value of the type its reader wants. -->
  <xsl:function name="eo:value" as="xs:string">
    <xsl:param name="token" as="xs:string"/>
    <xsl:param name="want" as="xs:string"/>
    <xsl:sequence select="if (eo:constant($token)) then eo:literal-of(eo:bytes($token), $want) else eo:cast(eo:local($token), eo:type($token), $want)"/>
  </xsl:function>
  <!-- The Java literal of bytes, as a value of a type. -->
  <xsl:function name="eo:literal-of" as="xs:string">
    <xsl:param name="bytes" as="xs:string"/>
    <xsl:param name="want" as="xs:string"/>
    <xsl:variable name="octets" select="tokenize($bytes, '-')[. != '']"/>
    <xsl:choose>
      <xsl:when test="not(matches($bytes, '^(--|([0-9A-Fa-f]{2}-)+|[0-9A-Fa-f]{2}(-[0-9A-Fa-f]{2})+)$'))">
        <xsl:sequence select="eo:taint(concat('The constant ', $bytes, ' of the entry ', $number, ' is not bytes'))"/>
      </xsl:when>
      <xsl:when test="$want = 'Phi'">
        <xsl:sequence select="concat('new Data.ToPhi(', eo:literal-of($bytes, 'byte[]'), ')')"/>
      </xsl:when>
      <xsl:when test="$want = 'double' and count($octets) = 8">
        <xsl:sequence select="concat('Double.longBitsToDouble(0x', upper-case(string-join($octets, '')), 'L)')"/>
      </xsl:when>
      <xsl:when test="$want = 'boolean' and upper-case($bytes) = ('FF-', '00-')">
        <xsl:sequence select="if (upper-case($bytes) = 'FF-') then 'true' else 'false'"/>
      </xsl:when>
      <xsl:when test="$want = 'byte[]' and empty($octets)">
        <xsl:sequence select="'new byte[0]'"/>
      </xsl:when>
      <xsl:when test="$want = 'byte[]'">
        <xsl:sequence select="concat('new byte[] {', string-join($octets ! concat('(byte) 0x', upper-case(.)), ', '), '}')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:taint(concat('The constant ', $bytes, ' of the entry ', $number, ' is no ', $want))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- The Java of a value of one type, read as a value of another one. -->
  <xsl:function name="eo:cast" as="xs:string">
    <xsl:param name="java" as="xs:string"/>
    <xsl:param name="from" as="xs:string"/>
    <xsl:param name="to" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="$from = $to">
        <xsl:sequence select="$java"/>
      </xsl:when>
      <xsl:when test="$to = 'Phi'">
        <xsl:sequence select="concat('new Data.ToPhi(', $java, ')')"/>
      </xsl:when>
      <xsl:when test="$from = 'Phi'">
        <xsl:sequence select="concat('new Dataized(', $java, ').', map {'byte[]': 'take()', 'double': 'asNumber()', 'boolean': 'asBool()'}($to))"/>
      </xsl:when>
      <xsl:when test="$from = 'byte[]' and $to = 'double'">
        <xsl:sequence select="concat('(double) new BytesOf(', $java, ').asNumber()')"/>
      </xsl:when>
      <xsl:when test="$from = 'double' and $to = 'byte[]'">
        <xsl:sequence select="concat('new BytesOf(', $java, ').take()')"/>
      </xsl:when>
      <xsl:when test="$from = 'boolean' and $to = 'byte[]'">
        <xsl:sequence select="concat('new byte[] {(byte) (', $java, ' ? 0xFF : 0x00)}')"/>
      </xsl:when>
      <xsl:when test="$from = 'byte[]' and $to = 'boolean'">
        <xsl:sequence select="concat($java, '[0] == -1')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:taint(concat('The ', $from, ' ', $java, ' of the entry ', $number, ' is read as a ', $to))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The statements of the symbols placed at one place, in the order they were
  minted, and then what stands at that place: the if of the fork there,
  with the blocks of its two sides inside, or the end of the path there. A
  place inside a side of the if of a joined symbol, which ends in the sign
  of that side and the symbol, holds its statements alone, since the if
  assigns the symbol right after them.
  -->
  <xsl:function name="eo:block" as="xs:string">
    <xsl:param name="at" as="map(xs:string, xs:string*)"/>
    <xsl:param name="place" as="xs:string*"/>
    <xsl:param name="depth" as="xs:integer"/>
    <xsl:variable name="key" select="eo:place($place)"/>
    <xsl:variable name="indent" select="string-join((1 to $depth) ! '    ', '')"/>
    <xsl:variable name="statements" select="string-join(sort(map:keys($at)[deep-equal($at(.), $place)], (), eo:serial#1) ! eo:statement($at, ., $depth), '')"/>
    <xsl:choose>
      <xsl:when test="starts-with($place[last()], '+') or starts-with($place[last()], '-')">
        <xsl:sequence select="$statements"/>
      </xsl:when>
      <xsl:when test="map:contains($eo:nodes, $key)">
        <xsl:variable name="condition" select="$eo:nodes($key)"/>
        <xsl:sequence select="concat($statements, $indent, 'if (', eo:value($condition, 'boolean'), ') {&#10;', eo:block($at, ($place, concat($condition, '+')), $depth + 1), $indent, '} else {&#10;', eo:block($at, ($place, concat($condition, '-')), $depth + 1), $indent, '}&#10;')"/>
      </xsl:when>
      <xsl:when test="map:contains($eo:leaves, $key)">
        <xsl:sequence select="concat($statements, eo:ending($eo:leaves($key), $depth))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' has no path that ends ', eo:at($key), ' and no fork there'))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- The statement a symbol is computed by. -->
  <xsl:function name="eo:statement" as="xs:string">
    <xsl:param name="at" as="map(xs:string, xs:string*)"/>
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:param name="depth" as="xs:integer"/>
    <xsl:variable name="indent" select="string-join((1 to $depth) ! '    ', '')"/>
    <xsl:variable name="type" select="eo:type($symbol)"/>
    <xsl:variable name="local" select="eo:local($symbol)"/>
    <xsl:variable name="joined" select="key('eo:joined', $symbol, $eo:doc)[1]"/>
    <xsl:choose>
      <xsl:when test="map:contains($eo:voids, $symbol)">
        <xsl:sequence select="concat($indent, 'final ', $type, ' ', $local, ' = new Dataized(', eo:object($symbol), ').', (map {'boolean': 'asBool()'}($type), 'take()')[1], ';&#10;')"/>
      </xsl:when>
      <xsl:when test="exists($joined)">
        <xsl:variable name="place" select="$at($symbol)"/>
        <xsl:sequence select="concat($indent, 'final ', $type, ' ', $local, ';&#10;', $indent, 'if (', eo:value(eo:condition($joined), 'boolean'), ') {&#10;', eo:block($at, ($place, concat('+', $symbol)), $depth + 1), $indent, '    ', $local, ' = ', eo:value(eo:branch($joined, 'left'), $type), ';&#10;', $indent, '} else {&#10;', eo:block($at, ($place, concat('-', $symbol)), $depth + 1), $indent, '    ', $local, ' = ', eo:value(eo:branch($joined, 'right'), $type), ';&#10;', $indent, '}&#10;')"/>
      </xsl:when>
      <xsl:when test="exists(key('eo:deferred', $symbol, $eo:doc))">
        <xsl:variable name="deferred" select="key('eo:deferred', $symbol, $eo:doc)[1]"/>
        <xsl:sequence select="concat($indent, 'final ', $type, ' ', $local, ' = new PhApplication(', string-join((eo:copied($deferred), eo:arguments($deferred) ! concat('new Bind(&quot;', eo:literal(substring-before(., ':')), '&quot;, ', eo:argument(substring-after(., ':')), ')')), ', '), ');&#10;')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="op" select="eo:operation($symbol)"/>
        <xsl:variable name="wants" select="tokenize($op/@args, ' ')"/>
        <xsl:variable name="operands" select="eo:operands($symbol)"/>
        <xsl:if test="count($operands) != count($wants)">
          <xsl:sequence select="eo:taint(concat('The symbol ', $symbol, ' of the entry ', $number, ' has ', count($operands), ' operands, while ', $op/@λ, ' takes ', count($wants)))"/>
        </xsl:if>
        <xsl:sequence select="concat($indent, 'final ', $type, ' ', $local, ' = ', fold-left(1 to count($wants), string($op), function($java, $i) { replace($java, concat('⟨', $i, '⟩'), eo:value($operands[$i], $wants[$i]), 'q') }), ';&#10;')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  Unicode escape of a character Java forbids in an identifier. Six digits,
  not four: four dropped everything above the sixteenth bit of a code point,
  so U+1F600 and U+F600 both read as "$uF600" (#9047).
  -->
  <xsl:function name="eo:escape-char" as="xs:string">
    <xsl:param name="c" as="xs:string"/>
    <xsl:variable name="code" select="string-to-codepoints($c)[1]"/>
    <xsl:value-of select="concat('$u', string-join(for $w in (1048576, 65536, 4096, 256, 16, 1) return substring('0123456789ABCDEF', ($code idiv $w) mod 16 + 1, 1), ''))"/>
  </xsl:function>
  <!-- Turn a name into a Java identifier, escaping every character Java forbids there -->
  <xsl:function name="eo:identifier" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:variable name="escaped">
      <xsl:analyze-string select="$n" regex="[^\p{{L}}\d_$]">
        <xsl:matching-substring>
          <xsl:value-of select="eo:escape-char(.)"/>
        </xsl:matching-substring>
        <xsl:non-matching-substring>
          <xsl:value-of select="."/>
        </xsl:non-matching-substring>
      </xsl:analyze-string>
    </xsl:variable>
    <xsl:value-of select="$escaped"/>
  </xsl:function>
  <!-- Turn a name into the body of a Java string literal, escaping the backslash and the quote -->
  <xsl:function name="eo:literal" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:value-of select="replace(replace($n, '\\', '\\\\'), '&quot;', '\\&quot;')"/>
  </xsl:function>
  <!--
  Get clean escaped object name. The "-" becomes "_" and the "_" an escape of
  its own, which no "-" can produce: mapping "_" to "__" beside them made
  "a-_b" and "a_-b" one name (#9047). The dollar is escaped ahead of them, so
  that the escape is never read as a dollar the name itself carried.
  -->
  <xsl:function name="eo:clean" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:value-of select="concat('EO', eo:identifier(replace(translate(translate(string-join(tokenize(replace($n, '\$', '\$EO'), '_'), eo:escape-char('_')), '-', '_'), '@', $eo:phi), $eo:alpha, '_')))"/>
  </xsl:function>
  <!--
  A deterministic digit fingerprint of a name, computed purely from the name's own
  characters rather than from any surrounding XML node. Two over-long names sharing
  their first 240-odd characters still disambiguate, and the same name always
  fingerprints the same way regardless of which call site of "eo:class-name" asks,
  so a declaration and a reference to the same over-long name never diverge (#7254).
  Two polynomial hashes with different bases and different prime moduli, since a
  single weighted sum of the code points cancels out for names that differ in two
  positions only (#7633).
  -->
  <xsl:function name="eo:fingerprint" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:variable name="codes" select="string-to-codepoints($n)"/>
    <xsl:value-of select="concat('_', string(eo:polynomial($codes, 131, 1000000007, 0)), '_', string(eo:polynomial($codes, 137, 998244353, 0)))"/>
  </xsl:function>
  <!--
  A polynomial hash of the code points, folded left to right, so that the same
  characters in another order hash differently.
  -->
  <xsl:function name="eo:polynomial" as="xs:integer">
    <xsl:param name="codes" as="xs:integer*"/>
    <xsl:param name="base" as="xs:integer"/>
    <xsl:param name="modulo" as="xs:integer"/>
    <xsl:param name="acc" as="xs:integer"/>
    <xsl:choose>
      <xsl:when test="empty($codes)">
        <xsl:sequence select="$acc"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:polynomial(subsequence($codes, 2), $base, $modulo, ($acc * $base + $codes[1]) mod $modulo)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  A cut prefix with any trailing dot dropped, so the digit-starting fingerprint
  appended after it lands inside an existing identifier segment instead of
  starting an illegal one of its own (#7254).
  -->
  <xsl:function name="eo:unbroken" as="xs:string">
    <xsl:param name="s" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="ends-with($s, '.')">
        <xsl:value-of select="eo:unbroken(substring($s, 1, string-length($s) - 1))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$s"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- Get class name for the object -->
  <xsl:function name="eo:class-name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:variable name="parts" select="tokenize($n, '\.')"/>
    <xsl:variable name="package">
      <xsl:for-each select="$parts">
        <xsl:if test="position()!=last()">
          <xsl:value-of select="eo:clean(.)"/>
          <xsl:text>.</xsl:text>
        </xsl:if>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="class">
      <xsl:choose>
        <xsl:when test="$parts[last()]">
          <xsl:value-of select="$parts[last()]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$parts"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="pre" select="concat($package, eo:clean($class))"/>
    <xsl:choose>
      <xsl:when test="string-length($pre)&gt;250">
        <xsl:variable name="fingerprint" select="eo:fingerprint($n)"/>
        <xsl:value-of select="concat(eo:unbroken(substring($pre, 1, 250 - string-length($fingerprint))), $fingerprint)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$pre"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- Get clean escaped package segment, prefixed to never clash with an object class, mapped the way "eo:clean" maps a name -->
  <xsl:function name="eo:clean-package" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:value-of select="concat('EO_', eo:identifier(replace(translate(translate(string-join(tokenize(replace($n, '\$', '\$EO'), '_'), eo:escape-char('_')), '-', '_'), '@', $eo:phi), $eo:alpha, '_')))"/>
  </xsl:function>
  <!-- Get Java package name for the EO package, one clean-package per segment -->
  <xsl:function name="eo:package-name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:variable name="joined">
      <xsl:for-each select="tokenize($n, '\.')">
        <xsl:if test="position()!=1">
          <xsl:text>.</xsl:text>
        </xsl:if>
        <xsl:value-of select="eo:clean-package(.)"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:value-of select="$joined"/>
  </xsl:function>
</xsl:stylesheet>
