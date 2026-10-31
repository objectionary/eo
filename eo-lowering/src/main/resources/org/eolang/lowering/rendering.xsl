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
  The protocol is the run of the entry, written as a tree. A run forks
  where a λ function branches on a value nobody worked out. phino morphs
  both sides first, and when they come to one term up to their symbols it
  joins them into a fresh symbol that stands for either, and the run goes
  on as one. Otherwise the run splits at the fork into two flows, and the
  record of the fork holds both: its left side holds what the flow that
  takes the left side did from the fork on, to the end of the run, and its
  right side the same for the other flow. A fork a flow meets inside a side
  is nested inside that side the same way, and a fork on a condition the
  flow decided before only says which side it takes again. The blocks the
  fork sat inside, the firings of λ functions, the dataizations and the
  morphs, close inside each side, where their records after the fork stand
  one after another, innermost block first. So a leaf of the tree, which
  is a side no fork splits, or the whole run when nothing forked, is one
  way the run went, named by the decisions of the forks above it, and the
  tree is a tree of ifs. The lambda of the atom is that tree: every if
  asks the condition of one fork, and every leaf of the tree returns what its flow came to, or throws
  the message its flow stopped at, when the side the flow took is a copy of
  "throw", which every "T" became before the world was merged. A flow that
  came to neither a root nor a throw makes the entry a taint, and so does
  a side phino never ran, as when it cuts the flows, and so does a throw
  whose message is no string literal.
  The protocol says what fired and off which symbols, and every symbol but
  a symbolized one is a private method of the atom, which computes it only
  when a reader calls it: a void is read off the object the atom lives in,
  a minted symbol is the operation of its λ, a symbolized one is a literal
  in the place of every call, a joined one is an if that returns one branch
  or the other, and a deferred one is the copy of an object of the entry with
  its arguments bound, which phino left for the run to work out. A loop
  phino cut gets no symbol, so a flow that met one makes the entry a taint.
  The copy is made where phino defers it, and dataized only where a λ reads
  it as an operand, since phino read it as data right there. The root of a flow is
  returned inside the object the protocol names around it, when the answer
  of the morph of the body is an object of the world applied to its "φ"
  alone, since such an object dataizes through its "φ" and the data of the
  root is the data of that "φ": a string, for example, is "Φ.string" applied
  to its bytes again, and a number is what "Data.ToPhi" makes of its double.
  Otherwise the root is returned as the object it is when it is a void or
  a copy, and bare when it is data the atom computed or a constant the flow
  came down to. Bare data loses the object around it, so then the tables of
  "eo:inference" are asked what the body returns, and an entry whose body is
  not a number, a bool or bytes is a taint: a string would lose every
  attribute of its own, and so would an "i16". Nothing is computed before a
  reader asks for it, so what one flow alone needs is computed on that flow
  alone, and the branch of a joined symbol that its condition does not take
  is never computed at all. An entry whose protocol defines one
  symbol twice is a taint, since phino promises to mint every symbol once
  and the atom could not tell which of the two values a reader of the symbol
  means.
  A taint is raised as an error and caught once, at the top, since there is
  no half of an atom worth writing.
  The class is named by "_java-names.xsl", the same functions the transpiler
  names every class with, because the transpiler names every reference to
  the atom by that rule and javac finds the class only under that one name.
  -->
  <xsl:import href="/org/eolang/lowering/_java-names.xsl"/>
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
  <!-- The table of rows the planting wrote, as a URI. -->
  <xsl:param name="rows" as="xs:string" select="''"/>
  <!--
  The directory with the tables of "eo:inference", as a URI, which holds
  "facts.xml" and "answers.xml". The tables are opened only when the root
  of a flow is data, since the tables of eo-runtime are large and nothing
  else asks about the body.
  -->
  <xsl:param name="inference" as="xs:string" select="''"/>
  <xsl:variable name="eo:tables" as="xs:string" select="if (ends-with($inference, '/')) then $inference else concat($inference, '/')"/>
  <xsl:variable name="eo:facts" as="document-node()" select="document(concat($eo:tables, 'facts.xml'))"/>
  <xsl:variable name="eo:answers" as="document-node()" select="document(concat($eo:tables, 'answers.xml'))"/>
  <xsl:variable name="eo:cactoos" select="'🌵'"/>
  <xsl:variable name="eo:alpha" select="'α'"/>
  <xsl:variable name="eo:phi" select="'φ'"/>
  <!--
  What every λ the atom can spell is: the Java type of what its operation
  computes, the Java types its operation reads its operands as, in the
  order phino lists them, and the Java of the operation with a numbered
  hole for each operand. These types live inside the operation alone: the
  symbol it mints is bytes, like every other symbol. Out of its range, the atom of
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
  <xsl:key name="eo:number" match="dataized[@number] | symbolized[@number]" use="upper-case(normalize-space(.))"/>
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
  <!--
  The rows of the table of operations the planting wrote for the formations
  of the world, by λ, each as the path of its formation and the name and
  the carrier of every operand it dataizes, with a colon between them.
  -->
  <xsl:variable name="eo:rows" as="map(xs:string, xs:string+)">
    <xsl:map>
      <xsl:for-each select="unparsed-text-lines($rows)[. != '']">
        <xsl:variable name="cells" select="tokenize(., '&#9;')"/>
        <xsl:map-entry key="$cells[1]" select="$cells[position() ge 3]"/>
      </xsl:for-each>
    </xsl:map>
  </xsl:variable>
  <xsl:variable name="eo:doc" select="/"/>
  <!--
  The dataization of the entry, which is the whole run: phino morphs the
  entry, takes its "φ", morphs that, which is the body, and goes on until it
  comes down to data, which is the root.
  -->
  <xsl:variable name="eo:run" as="element()?" select="$eo:doc/protocol/dataize"/>
  <!--
  The answer of the morph of the body, or nothing: the morph that phino
  starts on the object the first box of the run binds, since that box takes
  the "φ" of the entry, which is the body.
  -->
  <xsl:variable name="eo:answer" as="element()?" select="$eo:run/morph[@at = $eo:run/boxed[1]/@meta]/morphed[last()]"/>
  <!-- The sign a decision is spelled with, for every side a fork may take. -->
  <xsl:variable name="eo:signs" as="map(xs:string, xs:string)" select="map {'left': '+', 'right': '-'}"/>
  <!--
  The leaves of the tree of ifs: every side of a fork that no fork splits
  again, and the run itself when nothing forked, by the place it ends at,
  which is all of the decisions above it spelled one after another.
  -->
  <xsl:variable name="eo:leaves" as="map(xs:string, element())" select="map:merge(for $leaf in ($eo:run | $eo:doc//forked[@on]/(left | right))[not(.//forked[@on])] return map {eo:place(eo:where($leaf)): $leaf}, map {'duplicates': 'use-first'})"/>
  <!-- The forks of the tree of ifs: for every place a flow forked at, the condition it forked on there. -->
  <xsl:variable name="eo:nodes" as="map(xs:string, xs:string)" select="map:merge($eo:doc//forked[@on] ! map {eo:place(eo:where(.)): string(@on)}, map {'duplicates': 'use-first'})"/>
  <xsl:template match="/">
    <rendered>
      <xsl:try>
        <xsl:sequence select="eo:checked()"/>
        <xsl:variable name="reached" select="eo:reached()"/>
        <atom file="{eo:file()}" voids="{count($reached[map:contains($eo:voids, .)])}" statements="{count($reached[exists(key('eo:minted', ., $eo:doc)) or exists(key('eo:joined', ., $eo:doc)) or exists(key('eo:deferred', ., $eo:doc))])}" branches="{map:size($eo:nodes) + count($reached[exists(key('eo:joined', ., $eo:doc))])}">
          <xsl:value-of select="eo:java(eo:block((), 2), string-join($reached ! concat('&#10;', eo:method(.)), ''))"/>
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
  <!--
  The decisions above an element of the protocol, from the outermost fork
  in: the condition of every fork whose side the element stands inside,
  with "+" when that side is the left one and "-" when it is the right one.
  -->
  <xsl:function name="eo:where" as="xs:string*">
    <xsl:param name="element" as="element()"/>
    <xsl:sequence select="$element/ancestor-or-self::*[self::left or self::right] ! concat(../@on, $eo:signs(local-name()))"/>
  </xsl:function>
  <!--
  Whether an element of the protocol stands on the flow of a leaf, which
  is so when every side it stands inside is above the leaf as well.
  -->
  <xsl:function name="eo:on" as="xs:boolean">
    <xsl:param name="element" as="element()"/>
    <xsl:param name="leaf" as="element()"/>
    <xsl:sequence select="empty($element/ancestor::*[self::left or self::right] except $leaf/ancestor-or-self::*)"/>
  </xsl:function>
  <!-- A place in the tree of ifs, which is a list of decisions, as one string. -->
  <xsl:function name="eo:place" as="xs:string">
    <xsl:param name="decisions" as="xs:string*"/>
    <xsl:sequence select="string-join($decisions, ' ')"/>
  </xsl:function>
  <!-- The words that name a place in a taint. -->
  <xsl:function name="eo:at" as="xs:string">
    <xsl:param name="place" as="xs:string"/>
    <xsl:sequence select="if ($place = '') then 'before any decision' else concat('after ', $place)"/>
  </xsl:function>
  <!-- The words that name the flow of a leaf in a taint. -->
  <xsl:function name="eo:flow" as="xs:string">
    <xsl:param name="leaf" as="element()"/>
    <xsl:variable name="decisions" select="eo:where($leaf)"/>
    <xsl:sequence select="if (exists($decisions)) then concat('the flow that takes ', string-join($decisions, ' ')) else 'the only flow'"/>
  </xsl:function>
  <!--
  Stop the rendering of an entry whose protocol is no tree of ifs: one that
  ran out of time, holds no run, defines a symbol twice, forks on two
  conditions at one place, forks afresh on a condition the flow decided
  before, forks on a side phino never ran, as it does when "max-paths" cuts
  the run, or takes a side of a condition no fork above decided, or the
  other side of one a fork above did.
  -->
  <xsl:function name="eo:checked">
    <xsl:variable name="timeout" select="($eo:doc//timeout)[1]"/>
    <xsl:if test="exists($timeout)">
      <xsl:sequence select="eo:taint(concat('The entry ', $number, ' ran out of ', $timeout/@limit, ' seconds at ', $timeout/@at))"/>
    </xsl:if>
    <xsl:if test="empty($eo:run)">
      <xsl:sequence select="eo:taint(concat('The protocol of the entry ', $number, ' holds no run, while phino writes the dataization of every run'))"/>
    </xsl:if>
    <xsl:for-each-group select="$eo:doc//(minted | joined | symbolized | deferred)/@symbol" group-by=".">
      <xsl:if test="count(current-group()) &gt; 1">
        <xsl:sequence select="eo:taint(concat('The symbol ', current-grouping-key(), ' of the entry ', $number, ' is defined ', count(current-group()), ' times, while phino mints every symbol once'))"/>
      </xsl:if>
    </xsl:for-each-group>
    <xsl:for-each-group select="$eo:doc//forked[@on]" group-by="eo:place(eo:where(.))">
      <xsl:if test="count(current-group()) &gt; 1">
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' forks on ', string-join(current-group()/@on, ' and on '), ' ', eo:at(current-grouping-key()), ', while every run forks on one condition there'))"/>
      </xsl:if>
    </xsl:for-each-group>
    <xsl:for-each select="$eo:doc//forked[@on]">
      <xsl:variable name="place" select="eo:place(eo:where(.))"/>
      <xsl:if test="@on = ancestor::*[self::left or self::right]/../@on">
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' forks on ', @on, ' ', eo:at($place), ' afresh, while a fork above decided it'))"/>
      </xsl:if>
      <xsl:if test="empty(left) or empty(right)">
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' forks on ', @on, ' ', eo:at($place), ', while phino never ran its ', if (empty(left)) then 'left' else 'right', ' side, as it does when the flows are cut'))"/>
      </xsl:if>
    </xsl:for-each>
    <xsl:for-each select="$eo:doc//forked[@side]">
      <xsl:variable name="condition" select="normalize-space(.)"/>
      <xsl:variable name="place" select="eo:place(eo:where(.))"/>
      <xsl:variable name="decision" select="eo:where(.)[substring(., 1, string-length(.) - 1) = $condition]"/>
      <xsl:choose>
        <xsl:when test="not(map:contains($eo:signs, @side))">
          <xsl:sequence select="eo:taint(concat('The entry ', $number, ' takes the ', @side, ' side of ', $condition, ' ', eo:at($place), ', while a fork has a left and a right side'))"/>
        </xsl:when>
        <xsl:when test="empty($decision)">
          <xsl:sequence select="eo:taint(concat('The entry ', $number, ' takes the ', @side, ' side of ', $condition, ' ', eo:at($place), ', while no fork above decided ', $condition, ', as when phino cuts the flows it runs'))"/>
        </xsl:when>
        <xsl:when test="$decision != concat($condition, $eo:signs(@side))">
          <xsl:sequence select="eo:taint(concat('The entry ', $number, ' takes the ', @side, ' side of ', $condition, ' ', eo:at($place), ', while a fork above took the other side of it'))"/>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>
  <!--
  Stop the rendering of an entry with a flow that dataized no root, saying
  what the protocol shows instead: a body that was ⊥ from the start, the
  last step phino got no answer for, the loop phino cut, or the last λ phino
  got stuck at.
  -->
  <xsl:function name="eo:rootless">
    <xsl:param name="leaf" as="element()"/>
    <xsl:variable name="unanswered" select="string(($eo:doc//unanswered[eo:on(., $leaf)])[last()])"/>
    <xsl:variable name="looped" select="($eo:doc//looped[eo:on(., $leaf)])[last()]"/>
    <xsl:variable name="stuck" select="($eo:doc//stuck[@λ][eo:on(., $leaf)])[last()]"/>
    <xsl:variable name="prefix" select="concat('The entry ', $number, ' came to no root on ', eo:flow($leaf), ', because ')"/>
    <xsl:choose>
      <xsl:when test="normalize-space($eo:answer) = '⊥' or exists($eo:run/boxed[1]/following-sibling::*[1][self::unanswered][@λ = '⊥'])">
        <xsl:sequence select="eo:taint(concat('The body of the entry ', $number, ' reduced to ⊥ before phino computed anything, so its root was never dataized'))"/>
      </xsl:when>
      <xsl:when test="$unanswered = '⊥'">
        <xsl:sequence select="eo:taint(concat($prefix, 'phino came to a step that reduced to ⊥ and could not go on'))"/>
      </xsl:when>
      <xsl:when test="$unanswered != ''">
        <xsl:sequence select="eo:taint(concat($prefix, 'phino got no answer for ', $unanswered))"/>
      </xsl:when>
      <xsl:when test="exists($looped)">
        <xsl:sequence select="eo:taint(concat($prefix, 'phino cut a loop at ', $looped/@of, $stuck/@λ ! (' and got stuck at ' || .)))"/>
      </xsl:when>
      <xsl:when test="exists($stuck)">
        <xsl:sequence select="eo:taint(concat($prefix, 'phino got stuck at ', $stuck/@λ))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:taint(concat($prefix, 'its protocol says nothing about why'))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  Whether the run is still open where a leaf starts,
  as it is when a fork split the flow inside it, so that it closes inside
  the leaf. Every dataization still open inside it where the leaf starts
  closes inside the leaf as well, each with the record of what it came
  down to, and these records stand before the record of the root,
  innermost first.
  -->
  <xsl:function name="eo:split" as="xs:boolean">
    <xsl:param name="leaf" as="element()"/>
    <xsl:sequence select="exists($eo:run intersect $leaf/ancestor::*)"/>
  </xsl:function>
  <!-- The number of dataizations still open inside the run where a leaf starts. -->
  <xsl:function name="eo:opened" as="xs:integer">
    <xsl:param name="leaf" as="element()"/>
    <xsl:sequence select="count(eo:open($leaf) intersect $eo:run//dataize)"/>
  </xsl:function>
  <!--
  The dataizations still open where a side starts, innermost first. A side
  no side encloses starts with every dataization above it open. A side
  inside another one starts with those that opened inside the enclosing
  side after it started, and then with those open where the enclosing side
  started, less as many of them as the enclosing side closed before the
  fork, since every record of a closing closes the innermost one open.
  -->
  <xsl:function name="eo:open" as="element()*">
    <xsl:param name="side" as="element()"/>
    <xsl:variable name="enclosing" select="$side/ancestor::*[self::left or self::right][1]"/>
    <xsl:choose>
      <xsl:when test="empty($enclosing)">
        <xsl:sequence select="reverse($side/ancestor::dataize)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="(reverse($side/ancestor::dataize[. &gt;&gt; $enclosing]), subsequence(eo:open($enclosing), count($enclosing/(manufactured | dataized)[. &lt;&lt; $side]) + 1))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The firing a record belongs to: the one it stands inside, or, when it
  stands right inside a side, the one that went on after the fork: the
  firing of the fork itself when no dataization closed before the
  record, and otherwise the one the last dataization that closed before
  the record belongs to, which is the one its record stands inside, or
  belongs to the same way when that record stands right inside a side.
  -->
  <xsl:function name="eo:firing" as="element()">
    <xsl:param name="record" as="element()"/>
    <xsl:variable name="closed" select="count($record/preceding-sibling::*[self::manufactured or self::dataized])"/>
    <xsl:choose>
      <xsl:when test="$record/parent::evaluate">
        <xsl:sequence select="$record/.."/>
      </xsl:when>
      <xsl:when test="$closed = 0">
        <xsl:sequence select="$record/../../.."/>
      </xsl:when>
      <xsl:when test="$closed &gt; count(eo:open($record/..))">
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' closes a dataization on ', eo:flow($record/..), ' that it never opened'))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:firing(eo:open($record/..)[$closed])"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The record of the copy of "throw" a flow stopped at right where it
  dataized its root, or nothing: a child of that dataization, or, when the
  dataization closes inside the leaf, the record inside the leaf that
  stands after the records of the dataizations that opened inside it.
  -->
  <xsl:function name="eo:stop" as="element()?">
    <xsl:param name="leaf" as="element()"/>
    <xsl:choose>
      <xsl:when test="eo:split($leaf)">
        <xsl:sequence select="($leaf/unanswered[@λ = 'L_throw'][count(preceding-sibling::*[self::manufactured or self::dataized]) = eo:opened($leaf)])[1]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="($eo:run/unanswered[@λ = 'L_throw'])[1]"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The message a flow throws, as the Java of a string, or nothing when the
  flow does not throw. The flow throws when phino stopped at the λ of
  "throw" right where it dataized the root, and then the protocol names
  the copy of "throw" it stopped at, and the record that applied "throw"
  to its message. The message is a string literal when "eo:octets" finds
  its bytes, and those bytes are text in UTF-8, and then the atom throws
  that literal. A message that is anything else is an object, which the
  atom builds and dataizes into the reason it throws, the way the
  terminator of eo-runtime dataizes its own reason. An entry whose
  protocol does not say what the message is stays a taint.
  -->
  <xsl:function name="eo:thrown" as="xs:string?">
    <xsl:param name="leaf" as="element()"/>
    <xsl:variable name="stop" select="eo:stop($leaf)"/>
    <xsl:if test="exists($stop)">
      <xsl:variable name="message" select="normalize-space(($eo:doc//(applied | formed)[@meta = normalize-space($stop)]/*[@attr = 'message'])[1])"/>
      <xsl:variable name="bytes" select="eo:octets($message)"/>
      <xsl:variable name="prefix" select="concat('The entry ', $number, ' throws on ', eo:flow($leaf), ', but ')"/>
      <xsl:choose>
        <xsl:when test="$message = ''">
          <xsl:sequence select="eo:taint(concat($prefix, 'phino stopped at L_throw for ', normalize-space($stop), ', while no record of its protocol gives the message'))"/>
        </xsl:when>
        <xsl:when test="exists($bytes) and not(eo:text($bytes) = -1)">
          <xsl:sequence select="concat('&quot;', eo:message($bytes), '&quot;')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:sequence select="concat('new Dataized(', eo:computed($message, $prefix), ').asString()')"/>
        </xsl:otherwise>
      </xsl:choose>
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
    <xsl:variable name="string" select="($eo:doc//(applied | formed)[@meta = $message])[1][@of = 'Φ.string' and count(*) = 1]/meta[@attr = 'φ']"/>
    <xsl:variable name="bytes" select="normalize-space(($eo:doc//(applied | formed)[@meta = normalize-space($string)])[1][@of = 'Φ.bytes' and count(*) = 1]/datum[@attr = 'φ'])"/>
    <xsl:if test="matches($bytes, '^(--|[0-9A-F]{2}-|[0-9A-F]{2}(-[0-9A-F]{2})+)$')">
      <xsl:sequence select="$bytes"/>
    </xsl:if>
  </xsl:function>
  <!--
  The Java that builds the object a record of the protocol names, so that
  the atom can dataize it: an object of the world, outside the entry,
  applied to its "φ" alone, down to the bytes or the symbol at the bottom.
  A symbol is read the way a copy reads it, so a void arrives as the object
  it is and anything else as the value of its method. Bytes under
  "Φ.bytes" are written as "Data.ToPhi", which is "Φ.bytes" applied to
  them already, so the hop through it is left out. A record of any other
  shape names an object this stage cannot build, since it never parses a
  phi-expression, so its entry is a taint.
  -->
  <xsl:function name="eo:computed" as="xs:string">
    <xsl:param name="meta" as="xs:string"/>
    <xsl:param name="prefix" as="xs:string"/>
    <xsl:variable name="record" select="($eo:doc//(applied | formed)[@meta = $meta])[1]"/>
    <xsl:variable name="held" select="$record[matches(@of, '^Φ(\.[^.\s()]+)+$') and not(starts-with(@of, concat($locator, '.'))) and count(*) = 1]/*[@attr = 'φ']"/>
    <xsl:choose>
      <xsl:when test="empty($held)">
        <xsl:sequence select="eo:taint(concat($prefix, 'its message ', $meta, ' is no object of the world applied to its φ, so the atom cannot build it'))"/>
      </xsl:when>
      <xsl:when test="$held[self::datum] and $record/@of = 'Φ.bytes'">
        <xsl:sequence select="eo:literal-of(normalize-space($held), 'Phi')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="inner">
          <xsl:choose>
            <xsl:when test="$held[self::datum]">
              <xsl:sequence select="eo:literal-of(normalize-space($held), 'Phi')"/>
            </xsl:when>
            <xsl:when test="$held[self::meta]">
              <xsl:sequence select="eo:computed(normalize-space($held), $prefix)"/>
            </xsl:when>
            <xsl:when test="$held[self::symbol]">
              <xsl:sequence select="eo:argument(normalize-space($held))"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:sequence select="eo:taint(concat($prefix, 'the φ of its message ', $meta, ' is a ', name($held), ', which the atom cannot build'))"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:sequence select="concat('new PhApplication(', eo:global($record/@of), ', &quot;φ&quot;, ', $inner, ')')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  Every symbol the "φ" of a record names, down to the bottom, so that the
  atom computes them before it throws. Whether the record can be built at
  all is not asked here, since a record that cannot says so where it is
  built, and its entry is a taint either way.
  -->
  <xsl:function name="eo:borne" as="xs:string*">
    <xsl:param name="meta" as="xs:string"/>
    <xsl:variable name="held" select="($eo:doc//(applied | formed)[@meta = $meta])[1]/*[@attr = 'φ']"/>
    <xsl:sequence select="($held[self::symbol] ! normalize-space(.), $held[self::meta] ! eo:borne(normalize-space(.)))"/>
  </xsl:function>
  <!-- The symbols the message a flow throws is built from, or nothing when it throws none. -->
  <xsl:function name="eo:bearing" as="xs:string*">
    <xsl:param name="leaf" as="element()"/>
    <xsl:variable name="stop" select="eo:stop($leaf)"/>
    <xsl:if test="exists($stop)">
      <xsl:variable name="meta" select="normalize-space($stop)"/>
      <xsl:sequence select="eo:borne(normalize-space(($eo:doc//(applied | formed)[@meta = $meta]/*[@attr = 'message'])[1]))"/>
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
  The symbol the root of a flow dataizes, or nothing when the flow throws
  or came down to a constant. A flow that did none of these is a taint.
  -->
  <xsl:function name="eo:root" as="xs:string?">
    <xsl:param name="leaf" as="element()"/>
    <xsl:variable name="root" select="eo:rooted($leaf)[self::manufactured]"/>
    <xsl:choose>
      <xsl:when test="exists($root)">
        <xsl:variable name="symbol" select="string($root)"/>
        <xsl:if test="not(eo:constant($symbol)) and ((map:contains($eo:voids, $symbol) and exists(eo:returns($locator))) or (not(map:contains($eo:voids, $symbol)) and not(eo:type($symbol) = 'Phi'))) and empty($eo:around)">
          <xsl:sequence select="eo:bare($symbol)"/>
        </xsl:if>
        <xsl:sequence select="$symbol"/>
      </xsl:when>
      <xsl:when test="exists(eo:stop($leaf)) or exists(eo:fixed($leaf))"/>
      <xsl:otherwise>
        <xsl:sequence select="eo:rootless($leaf)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The record of what the root of a flow came down to, a symbol or its
  bytes, which closes the run, or nothing when the flow stopped inside the dataization of the root. The
  record is the last child of that dataization when the dataization is
  whole, and stands inside the leaf after the records of the dataizations
  that opened inside it when it closes there.
  -->
  <xsl:function name="eo:rooted" as="element()?">
    <xsl:param name="leaf" as="element()"/>
    <xsl:choose>
      <xsl:when test="eo:split($leaf)">
        <xsl:sequence select="($leaf/(manufactured | dataized))[eo:opened($leaf) + 1]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="$eo:run/(manufactured | dataized)[last()]"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The bytes a flow came down to right where it dataized its root, or
  nothing when it dataized a symbol there, or stopped.
  -->
  <xsl:function name="eo:fixed" as="xs:string?">
    <xsl:param name="leaf" as="element()"/>
    <xsl:variable name="bytes" select="normalize-space(eo:rooted($leaf)[self::dataized])"/>
    <xsl:if test="$bytes != ''">
      <xsl:sequence select="$bytes"/>
    </xsl:if>
  </xsl:function>
  <!--
  Stop the rendering of a flow whose root is data the atom would return
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
  The Java type of the data a flow came down to, as the tables of
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
  the answer of the morph of the body, or nothing: an object of the world,
  outside the entry, applied to its "φ" alone. Such an object dataizes
  through its "φ", so the data of the root is the data of that "φ", and the
  atom gives the object back by applying it to the root again. An answer
  that names anything else, an object with more arguments, one inside the
  entry, the carrier of a bool, or the application of a dispatch, names no
  such object, and then the root is returned the way its own type tells.
  The body is morphed before the run forks, so every flow has the same one.
  -->
  <xsl:variable name="eo:around" as="xs:string?">
    <xsl:variable name="applied" select="($eo:doc//(applied | formed)[@meta = normalize-space($eo:answer)])[1]"/>
    <xsl:if test="exists($applied) and count($applied/*) = 1 and $applied/*/@attr = 'φ' and matches($applied/@of, '^Φ(\.[^.\s()]+)+$') and not(starts-with($applied/@of, concat($locator, '.')))">
      <xsl:sequence select="string($applied/@of)"/>
    </xsl:if>
  </xsl:variable>
  <!-- The objects "Data.ToPhi" makes of a Java value, each with the type of that value. -->
  <xsl:variable name="eo:carriers" as="map(xs:string, xs:string)" select="map {'Φ.number': 'double', 'Φ.bytes': 'byte[]'}"/>
  <!--
  The Java of the root of a flow as the object the atom returns: inside the
  object the protocol names around it, or, when the protocol names none, a
  void and a copy as they are, and data wrapped into the carrier the tables
  of "eo:inference" give the body, since bytes alone do not say what they
  are.
  -->
  <xsl:function name="eo:returned" as="xs:string">
    <xsl:param name="root" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="empty($eo:around) and (map:contains($eo:voids, $root) or eo:type($root) = 'Phi')">
        <xsl:sequence select="eo:argument($root)"/>
      </xsl:when>
      <xsl:when test="empty($eo:around)">
        <xsl:sequence select="concat('new Data.ToPhi(', eo:value($root, eo:carrier($root)), ')')"/>
      </xsl:when>
      <xsl:when test="map:contains($eo:carriers, $eo:around) and not(map:contains($eo:voids, $root)) and not(eo:type($root) = 'Phi')">
        <xsl:sequence select="concat('new Data.ToPhi(', eo:value($root, $eo:carriers($eo:around)), ')')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="concat('new PhApplication(', eo:global($eo:around), ', &quot;φ&quot;, ', eo:argument($root), ')')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The Java of the bytes a flow came down to as the object the atom returns:
  inside the object the protocol names around them, or, when the protocol
  names none, wrapped into the carrier the tables of "eo:inference" give
  the body.
  -->
  <xsl:function name="eo:constant-returned" as="xs:string">
    <xsl:param name="bytes" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="empty($eo:around)">
        <xsl:sequence select="concat('new Data.ToPhi(', eo:literal-of($bytes, eo:carrier($bytes)), ')')"/>
      </xsl:when>
      <xsl:when test="map:contains($eo:carriers, $eo:around)">
        <xsl:sequence select="concat('new Data.ToPhi(', eo:literal-of($bytes, $eo:carriers($eo:around)), ')')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="concat('new PhApplication(', eo:global($eo:around), ', &quot;φ&quot;, ', eo:literal-of($bytes, 'Phi'), ')')"/>
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
  flow came to, or the throw of the message the flow stopped at.
  -->
  <xsl:function name="eo:ending" as="xs:string">
    <xsl:param name="leaf" as="element()"/>
    <xsl:param name="depth" as="xs:integer"/>
    <xsl:variable name="indent" select="string-join((1 to $depth) ! '    ', '')"/>
    <xsl:variable name="thrown" select="eo:thrown($leaf)"/>
    <xsl:variable name="root" select="eo:root($leaf)"/>
    <xsl:choose>
      <xsl:when test="exists($thrown)">
        <xsl:sequence select="concat($indent, 'throw new ExFailure(&quot;%s&quot;, ', $thrown, ');&#10;')"/>
      </xsl:when>
      <xsl:when test="exists($root) and eo:constant($root)">
        <xsl:sequence select="concat($indent, 'return ', eo:constant-returned(eo:bytes($root)), ';&#10;')"/>
      </xsl:when>
      <xsl:when test="exists($root)">
        <xsl:sequence select="concat($indent, 'return ', eo:returned($root), ';&#10;')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="concat($indent, 'return ', eo:constant-returned(eo:fixed($leaf)), ';&#10;')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The whole Java file of the atom, around the body of its lambda and the
  methods of its symbols, each of which starts with an empty line.
  -->
  <xsl:function name="eo:java" as="xs:string">
    <xsl:param name="body" as="xs:string"/>
    <xsl:param name="methods" as="xs:string"/>
    <xsl:variable name="class" select="eo:class()"/>
    <xsl:sequence select="string-join(('/*', ' * This file was generated by eo-lowering, from the protocol of the entry', ' * ' || $number || ', which is {@code ' || $locator || '}.', ' * This code is not optimized, on purpose: it keeps a method for every', ' * symbol and a branch for every fork, even a branch that can never be', ' * taken. The Java compiler and the JIT are expected to optimize it.', ' */', 'package ' || eo:package() || ';', '', 'import org.eolang.*;', '', '/**', ' * The atom that took the place of the body of {@code ' || $locator || '}.', ' */', '@XmirObject(oname = &quot;' || eo:literal(string-join(eo:names(), '.')) || '&quot;)', 'public final class ' || $class || ' extends PhDefault implements Atom {', '    public ' || $class || '() {', '        super(new Attrs(new Attr(Phi.RHO, new AtRho())));', '    }', '', '    @Override', '    public Phi lambda() {', $body || '    }' || replace('&#10;' || $methods, '&#10;$', ''), '}', ''), '&#10;')"/>
  </xsl:function>
  <!--
  Every symbol some flow reaches, in the order phino minted them. The roots
  are read at the leaves, and the condition of every fork where it forks.
  The symbols are walked from the highest number down, and each one reached
  reaches the symbols it reads. A root that is a void is returned as the
  object it is, and is never read as data at all, and neither is a root
  that is a constant. A joined symbol reads its condition and both of its
  branches, and a deferred one the arguments it binds that are no voids,
  since a void bound to a copy is the object it is as well.
  -->
  <xsl:function name="eo:reached" as="xs:string*">
    <xsl:variable name="all" select="distinct-values(($eo:doc//(minted | joined | deferred)/@symbol, map:keys($eo:voids)))"/>
    <xsl:variable name="seeds" as="xs:string*" select="(map:keys($eo:leaves) ! (eo:root($eo:leaves(.)), eo:bearing($eo:leaves(.)))[not(map:contains($eo:voids, .)) and not(eo:constant(.))], map:keys($eo:nodes) ! $eo:nodes(.)[not(eo:constant(.))])"/>
    <xsl:iterate select="sort($all, (), function($s) { -eo:serial($s) })">
      <xsl:param name="reached" as="xs:string*" select="distinct-values($seeds)"/>
      <xsl:on-completion>
        <xsl:for-each select="$reached[not(. = $all)][1]">
          <xsl:sequence select="eo:taint(concat('The symbol ', ., ' of the entry ', $number, ' is read, while nothing defines it'))"/>
        </xsl:for-each>
        <xsl:sequence select="sort($reached, (), eo:serial#1)"/>
      </xsl:on-completion>
      <xsl:variable name="symbol" select="."/>
      <xsl:variable name="reads" as="xs:string*">
        <xsl:choose>
          <xsl:when test="not($symbol = $reached)"/>
          <xsl:when test="exists(key('eo:joined', $symbol, $eo:doc))">
            <xsl:variable name="joined" select="key('eo:joined', $symbol, $eo:doc)[1]"/>
            <xsl:sequence select="(eo:condition($joined), eo:branch($joined, 'left'), eo:branch($joined, 'right'))"/>
          </xsl:when>
          <xsl:when test="exists(key('eo:deferred', $symbol, $eo:doc))">
            <xsl:sequence select="eo:arguments(key('eo:deferred', $symbol, $eo:doc)[1]) ! substring-after(., ':')[not(map:contains($eo:voids, .))]"/>
          </xsl:when>
          <xsl:when test="map:contains($eo:voids, $symbol)"/>
          <xsl:otherwise>
            <xsl:sequence select="eo:operands($symbol)"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="symbols" select="$reads[not(eo:constant(.))]"/>
      <xsl:for-each select="$symbols[eo:serial(.) &gt;= eo:serial($symbol)][1]">
        <xsl:sequence select="eo:taint(concat('The symbol ', $symbol, ' of the entry ', $number, ' reads ', ., ', which was not minted before it'))"/>
      </xsl:for-each>
      <xsl:next-iteration>
        <xsl:with-param name="reached" select="distinct-values(($reached, $symbols))"/>
      </xsl:next-iteration>
    </xsl:iterate>
  </xsl:function>
  <!-- The number of a symbol, which is the order phino minted it in. -->
  <xsl:function name="eo:serial" as="xs:integer">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:if test="not(matches($symbol, '^𝜎\d+$'))">
      <xsl:sequence select="eo:taint(concat('The symbol ', $symbol, ' of the entry ', $number, ' has no number'))"/>
    </xsl:if>
    <xsl:sequence select="xs:integer(substring($symbol, 2))"/>
  </xsl:function>
  <!--
  The name of the Java method that computes a symbol, which is the symbol
  itself, so that the Java reads as the protocol does.
  -->
  <xsl:function name="eo:name" as="xs:string">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:sequence select="$symbol"/>
  </xsl:function>
  <!-- The Java of a symbol read where the atom needs it: the call of its method. -->
  <xsl:function name="eo:call" as="xs:string">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:sequence select="concat('this.', eo:name($symbol), '()')"/>
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
  <!--
  The condition a joined symbol depends on. phino names it by the meta its
  dataization was bound to, so the condition is the symbol or the bytes
  that dataization came down to.
  -->
  <xsl:function name="eo:condition" as="xs:string">
    <xsl:param name="joined" as="element()"/>
    <xsl:variable name="bound" select="(root($joined)//bound[@meta = $joined/@on][. &lt;&lt; $joined])[last()]"/>
    <xsl:sequence select="if (exists($bound)) then normalize-space((root($bound)//(manufactured | dataized)[@meta = normalize-space($bound)][. &lt;&lt; $bound])[last()]) else string($joined/@on)"/>
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
      <xsl:sequence select="eo:taint(concat('The deferred symbol ', $deferred/@symbol, ' of the entry ', $number, ' has the argument ', @attr, ', which is no symbol'))"/>
    </xsl:for-each>
    <xsl:sequence select="$deferred/with/symbol ! concat(@attr, ':', normalize-space(.))"/>
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
      <xsl:when test="$names[1] = ('φ', 'body🌵')">
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
  <!-- The λ a minted symbol was minted by. -->
  <xsl:function name="eo:lambda" as="xs:string">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:sequence select="string(eo:firing(key('eo:minted', $symbol, $eo:doc)[1])/@λ)"/>
  </xsl:function>
  <!--
  The Java of the object a symbol the λ of a row minted stands for the
  dataization of: the formation of the row, reached from the root of the
  universe, or taken off its "ρ" when the row dataized one, with every
  void of it bound to the operand the row dataized for it. An operand is
  wrapped back into the carrier the row names for it, the way the planting
  wraps a void, so that the body of the formation dispatches on it as it
  would on a literal, and the call computes the same bytes whether that
  formation became an atom or stayed in EO.
  -->
  <xsl:function name="eo:rowed" as="xs:string">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:variable name="row" select="$eo:rows(eo:lambda($symbol))"/>
    <xsl:variable name="takes" select="$row[position() gt 1]"/>
    <xsl:variable name="operands" select="eo:operands($symbol)"/>
    <xsl:if test="count($operands) != count($takes)">
      <xsl:sequence select="eo:taint(concat('The symbol ', $symbol, ' of the entry ', $number, ' has ', count($operands), ' operands, while ', eo:lambda($symbol), ' takes ', count($takes)))"/>
    </xsl:if>
    <xsl:variable name="held" select="starts-with($takes[1], 'ρ:')"/>
    <xsl:variable name="object" select="if ($held) then concat(eo:wrapped($operands[1], substring-after($takes[1], ':')), '.take(&quot;', eo:literal(tokenize($row[1], '\.')[last()]), '&quot;)') else eo:global($row[1])"/>
    <xsl:variable name="binds" select="for $i in (if ($held) then 2 else 1) to count($takes) return concat('new Bind(&quot;', eo:literal(substring-before($takes[$i], ':')), '&quot;, ', eo:wrapped($operands[$i], substring-after($takes[$i], ':')), ')')"/>
    <xsl:sequence select="if (empty($binds)) then $object else concat('new PhApplication(', string-join(($object, $binds), ', '), ')')"/>
  </xsl:function>
  <!-- The Java of a token as the object of a carrier, the way "Data.ToPhi" and the library make one of data. -->
  <xsl:function name="eo:wrapped" as="xs:string">
    <xsl:param name="token" as="xs:string"/>
    <xsl:param name="carrier" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="$carrier = 'number'">
        <xsl:sequence select="concat('new Data.ToPhi(', eo:value($token, 'double'), ')')"/>
      </xsl:when>
      <xsl:when test="$carrier = 'bool'">
        <xsl:sequence select="concat('new Data.ToPhi(', eo:value($token, 'boolean'), ')')"/>
      </xsl:when>
      <xsl:when test="$carrier = 'bytes'">
        <xsl:sequence select="eo:value($token, 'Phi')"/>
      </xsl:when>
      <xsl:when test="$carrier = 'string'">
        <xsl:sequence select="concat('new PhApplication(', eo:global('Φ.string'), ', &quot;φ&quot;, ', eo:value($token, 'Phi'), ')')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:taint(concat('The operand ', $token, ' of the entry ', $number, ' is wrapped as a ', $carrier, ', which is no carrier'))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- The operation a minted symbol was minted by. -->
  <xsl:function name="eo:operation" as="element()">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:variable name="lambda" select="eo:lambda($symbol)"/>
    <xsl:variable name="op" select="$eo:operations[@λ = $lambda]"/>
    <xsl:if test="empty($op)">
      <xsl:sequence select="eo:taint(concat('The symbol ', $symbol, ' of the entry ', $number, ' is minted by ', $lambda, ', which has no Java'))"/>
    </xsl:if>
    <xsl:sequence select="$op"/>
  </xsl:function>
  <!--
  The Java type of a symbol. A symbol is data, and data in EO is bytes, so
  every symbol is bytes, whatever minted it: an operation reads its bytes
  as a double or a bool only inside its own Java, and writes what it
  computes back into bytes. A copy is an object, and so is a joined symbol
  with a copy in one of its branches, since it holds the object of the
  branch taken.
  -->
  <xsl:function name="eo:type" as="xs:string">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:variable name="joined" select="key('eo:joined', $symbol, $eo:doc)[1]"/>
    <xsl:choose>
      <xsl:when test="exists(key('eo:deferred', $symbol, $eo:doc))">
        <xsl:sequence select="'Phi'"/>
      </xsl:when>
      <xsl:when test="exists($joined) and (eo:branch($joined, 'left'), eo:branch($joined, 'right'))[not(eo:constant(.))] ! eo:type(.) = 'Phi'">
        <xsl:sequence select="'Phi'"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="'byte[]'"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- The Java of a token, as a value of the type its reader wants. -->
  <xsl:function name="eo:value" as="xs:string">
    <xsl:param name="token" as="xs:string"/>
    <xsl:param name="want" as="xs:string"/>
    <xsl:sequence select="if (eo:constant($token)) then eo:literal-of(eo:bytes($token), $want) else eo:cast(eo:call($token), eo:type($token), $want)"/>
  </xsl:function>
  <!--
  The Java literal of bytes, as a value of a type. A double is the number
  phino wrote next to the same bytes in the protocol, the shortest decimal
  that reads back as the same double, with the suffix "d", so that a reader
  sees at once that it is a double. It is the bits themselves when phino
  wrote no such number, or wrote one that is not finite.
  -->
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
        <xsl:variable name="number" select="string(key('eo:number', upper-case($bytes), $eo:doc)[1]/@number)"/>
        <xsl:sequence select="if (matches($number, '^-?[0-9]+\.[0-9]+(e-?[0-9]+)?$')) then $number || 'd' else 'Double.longBitsToDouble(0x' || upper-case(string-join($octets, '')) || 'L)'"/>
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
        <xsl:sequence select="concat('java.nio.ByteBuffer.wrap(', $java, ').getDouble()')"/>
      </xsl:when>
      <xsl:when test="$from = 'double' and $to = 'byte[]'">
        <xsl:sequence select="concat('java.nio.ByteBuffer.allocate(8).putDouble(', $java, ').array()')"/>
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
  What stands at one place of the tree of ifs: the if of the fork there,
  with the blocks of its two sides inside, or the end of the flow there.
  -->
  <xsl:function name="eo:block" as="xs:string">
    <xsl:param name="place" as="xs:string*"/>
    <xsl:param name="depth" as="xs:integer"/>
    <xsl:variable name="key" select="eo:place($place)"/>
    <xsl:variable name="indent" select="string-join((1 to $depth) ! '    ', '')"/>
    <xsl:choose>
      <xsl:when test="map:contains($eo:nodes, $key)">
        <xsl:variable name="condition" select="$eo:nodes($key)"/>
        <xsl:sequence select="concat($indent, 'if (', eo:value($condition, 'boolean'), ') {&#10;', eo:block(($place, $condition || '+'), $depth + 1), $indent, '} else {&#10;', eo:block(($place, $condition || '-'), $depth + 1), $indent, '}&#10;')"/>
      </xsl:when>
      <xsl:when test="map:contains($eo:leaves, $key)">
        <xsl:sequence select="eo:ending($eo:leaves($key), $depth)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' has no flow that ends ', eo:at($key), ' and no fork there'))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  The private method that computes a symbol, which runs only when a reader
  calls it, so that a symbol only one side of an if reads is computed on
  that side alone. A joined symbol asks its condition and computes the
  branch of the side it takes, and nothing of the other side. A symbol the
  λ of a row minted dataizes the object of the row, and a symbol any other
  λ minted is the operation of that λ over its operands.
  -->
  <xsl:function name="eo:method" as="xs:string">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:variable name="type" select="eo:type($symbol)"/>
    <xsl:variable name="joined" select="key('eo:joined', $symbol, $eo:doc)[1]"/>
    <xsl:variable name="body">
      <xsl:choose>
        <xsl:when test="map:contains($eo:voids, $symbol)">
          <xsl:sequence select="concat('        return new Dataized(', eo:object($symbol), ').take();&#10;')"/>
        </xsl:when>
        <xsl:when test="exists($joined)">
          <xsl:sequence select="concat('        if (', eo:value(eo:condition($joined), 'boolean'), ') {&#10;', '            return ', eo:value(eo:branch($joined, 'left'), $type), ';&#10;', '        } else {&#10;', '            return ', eo:value(eo:branch($joined, 'right'), $type), ';&#10;', '        }&#10;')"/>
        </xsl:when>
        <xsl:when test="exists(key('eo:deferred', $symbol, $eo:doc))">
          <xsl:variable name="deferred" select="key('eo:deferred', $symbol, $eo:doc)[1]"/>
          <xsl:sequence select="concat('        return new PhApplication(', string-join((eo:copied($deferred), eo:arguments($deferred) ! ('new Bind(&quot;' || eo:literal(substring-before(., ':')) || '&quot;, ' || eo:argument(substring-after(., ':')) || ')')), ', '), ');&#10;')"/>
        </xsl:when>
        <xsl:when test="map:contains($eo:rows, eo:lambda($symbol))">
          <xsl:sequence select="concat('        return new Dataized(', eo:rowed($symbol), ').take();&#10;')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="op" select="eo:operation($symbol)"/>
          <xsl:variable name="wants" select="tokenize($op/@args, ' ')"/>
          <xsl:variable name="operands" select="eo:operands($symbol)"/>
          <xsl:if test="count($operands) != count($wants)">
            <xsl:sequence select="eo:taint(concat('The symbol ', $symbol, ' of the entry ', $number, ' has ', count($operands), ' operands, while ', $op/@λ, ' takes ', count($wants)))"/>
          </xsl:if>
          <xsl:sequence select="concat('        return ', eo:cast(fold-left(1 to count($wants), string($op), function($java, $i) { replace($java, '⟨' || $i || '⟩', eo:value($operands[$i], $wants[$i]), 'q') }), string($op/@type), $type), ';&#10;')"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:sequence select="concat('    private ', $type, ' ', eo:name($symbol), '() {&#10;', $body, '    }&#10;')"/>
  </xsl:function>
</xsl:stylesheet>
