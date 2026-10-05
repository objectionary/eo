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
  computes, and branches it forks into, or "taint", holding why there is none.
  The protocol says what fired and off which symbols, and a symbol is all a
  Java local is: a void is read off the object the atom lives in, a minted
  symbol is one statement under the operation of its λ, a known one is a
  literal, a joined one is a blank final that each branch of an "if"
  assigns, and a deferred one is the dataized copy of an object of the entry
  with its arguments bound, which phino left for the run to work out. A
  looped one, which phino answered when it cut a loop, is the same. Such a
  copy is dataized only when the tables of "eo:inference" say that the
  object returns a number, a string, a bool or bytes, since dataizing any
  other object may do what EO never does there. A statement is placed as
  deep inside the branches as all of its readers let it, so that what one
  branch alone needs is computed in that branch alone. The symbols are
  walked from the highest number down, since phino mints a symbol only after
  the symbols it is made of, so by the time a symbol is reached every symbol
  that reads it has already said where it is read.
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
  opened only when an entry has a deferred symbol, since the table of
  eo-runtime is large and most entries have none.
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
    <op λ="L_bytes_right" type="byte[]" args="byte[] double">new BytesOf(⟨1⟩).shift((int) ⟨2⟩).take()</op>
    <op λ="L_bytes_concat" type="byte[]" args="byte[] byte[]">java.nio.ByteBuffer.allocate(⟨1⟩.length + ⟨2⟩.length).put(⟨1⟩).put(⟨2⟩).array()</op>
    <op λ="L_bytes_slice" type="byte[]" args="byte[] double double">new Slice(⟨1⟩, ⟨2⟩, ⟨3⟩).delta()</op>
    <op λ="L_dataized" type="byte[]" args="byte[]">⟨1⟩</op>
  </xsl:variable>
  <xsl:key name="eo:minted" match="minted" use="@symbol"/>
  <xsl:key name="eo:known" match="known" use="@symbol"/>
  <xsl:key name="eo:joined" match="joined" use="@symbol"/>
  <xsl:key name="eo:deferred" match="deferred | looped[@symbol]" use="@symbol"/>
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
  <xsl:template match="/">
    <rendered>
      <xsl:try>
        <xsl:variable name="root" select="eo:root()"/>
        <xsl:variable name="at" select="eo:placed($root)"/>
        <atom file="{eo:file()}" voids="{count(map:keys($at)[map:contains($eo:voids, .)])}" statements="{count(map:keys($at)[exists(key('eo:minted', ., $eo:doc)) or exists(key('eo:deferred', ., $eo:doc))])}" branches="{count(map:keys($at)[exists(key('eo:joined', ., $eo:doc))])}">
          <xsl:value-of select="eo:java($root, $at)"/>
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
  Stop the rendering of an entry whose root dataized no symbol, saying what
  the protocol shows instead: a constant the root dataized, a body that was
  ⊥ from the start, the last step phino got no answer for, or the last λ
  phino left unfinished.
  -->
  <xsl:function name="eo:rootless">
    <xsl:variable name="constant" select="($eo:doc/protocol/morph/evaluate[@λ = 'L_root'])[last()]/bind[starts-with(@meta, '𝛿1.')][last()]"/>
    <xsl:variable name="unanswered" select="string(($eo:doc//unanswered)[last()])"/>
    <xsl:choose>
      <xsl:when test="exists($constant)">
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' always gives the constant ', $constant, ', and an atom that only returns a constant is not written yet'))"/>
      </xsl:when>
      <xsl:when test="empty($eo:doc/protocol/morph/evaluate[@λ = 'L_root']) and $eo:doc/protocol/morph/evaluate[@λ = 'L_entry']/bind[@meta = '𝑛1.1'] = '⊥'">
        <xsl:sequence select="eo:taint(concat('The body of the entry ', $number, ' reduced to ⊥ before phino computed anything, so its root was never dataized'))"/>
      </xsl:when>
      <xsl:when test="$unanswered = '⊥'">
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' came to no root, because phino came to a step that reduced to ⊥ and could not go on'))"/>
      </xsl:when>
      <xsl:when test="ends-with($unanswered, ':λ')">
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' came to no root, because phino got no answer from ', substring-before($unanswered, ':λ')))"/>
      </xsl:when>
      <xsl:when test="$unanswered != ''">
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' came to no root, because phino got no answer for ', $unanswered))"/>
      </xsl:when>
      <xsl:when test="exists($eo:doc//unfinished[@λ])">
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' came to no root, because phino left ', ($eo:doc//unfinished[@λ])[last()]/@λ, ' unfinished'))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:taint(concat('The entry ', $number, ' came to no root, while its protocol says nothing about why'))"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- The symbol the root of the entry dataizes. -->
  <xsl:function name="eo:root" as="xs:string">
    <xsl:variable name="timeout" select="($eo:doc//timeout)[1]"/>
    <xsl:if test="exists($timeout)">
      <xsl:sequence select="eo:taint(concat('The entry ', $number, ' ran out of ', $timeout/@limit, ' seconds at ', $timeout/@at))"/>
    </xsl:if>
    <xsl:variable name="root" select="$eo:doc/protocol/morph/evaluate[@λ = 'L_root']/dataize[starts-with(@meta, '𝛿1.')][last()]"/>
    <xsl:if test="empty($root)">
      <xsl:sequence select="eo:rootless()"/>
    </xsl:if>
    <xsl:variable name="symbol" select="substring-before(concat(string($root), ':'), ':')"/>
    <xsl:if test="exists(key('eo:known', $symbol, $eo:doc))">
      <xsl:sequence select="eo:taint(concat('The root ', $symbol, ' of the entry ', $number, ' is a constant'))"/>
    </xsl:if>
    <xsl:sequence select="$symbol"/>
  </xsl:function>
  <!-- The object a void is, as a chain of takes off the atom. -->
  <xsl:function name="eo:object" as="xs:string">
    <xsl:param name="void" as="xs:string"/>
    <xsl:sequence select="string-join(('this.take(&quot;ρ&quot;)', tokenize($eo:voids($void)[1], '\.') ! concat('.take(&quot;', eo:literal(.), '&quot;)')), '')"/>
  </xsl:function>
  <!-- The whole Java file of the atom. -->
  <xsl:function name="eo:java" as="xs:string">
    <xsl:param name="root" as="xs:string"/>
    <xsl:param name="at" as="map(xs:string, xs:string*)"/>
    <xsl:variable name="body">
      <xsl:choose>
        <xsl:when test="map:contains($eo:voids, $root)">
          <xsl:value-of select="concat('        return ', eo:object($root), ';&#10;')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="concat(eo:block($at, (), 2), '        return new Data.ToPhi(', eo:local($root), ');&#10;')"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="class" select="eo:class()"/>
    <xsl:sequence select="string-join(('/*', ' * This file was generated by eo-lowering, from the protocol of the entry', concat(' * ', $number, ', which is ', $locator, '.'), ' */', concat('package ', eo:package(), ';'), '', 'import org.eolang.*;', '', '/**', concat(' * The atom that took the place of the body of ', $locator, '.'), ' */', concat('@XmirObject(oname = &quot;', eo:literal(string-join(eo:names(), '.')), '&quot;)'), concat('public final class ', $class, ' extends PhDefault implements Atom {'), concat('    public ', $class, '() {'), '        super(new Attrs(new Attr(Phi.RHO, new AtRho())));', '    }', '', '    @Override', '    public Phi lambda() {', concat($body, '    }'), '}', ''), '&#10;')"/>
  </xsl:function>
  <!--
  Where every symbol the root reaches is placed, as a map from the symbol to
  its place: the "if" branches it sits inside, from the outermost in, each
  one as the joined symbol with "+" for its left branch and "-" for its right
  one. The symbols are walked from the highest number down, and each one
  tells the symbols it reads where it reads them, so that a symbol read in
  two places is placed where both places agree.
  -->
  <xsl:function name="eo:placed" as="map(xs:string, xs:string*)">
    <xsl:param name="root" as="xs:string"/>
    <xsl:variable name="all" select="distinct-values(($eo:doc//(minted | joined | deferred | looped)/@symbol, map:keys($eo:voids)))"/>
    <xsl:iterate select="sort($all, (), function($s) { -eo:serial($s) })">
      <xsl:param name="at" as="map(xs:string, xs:string*)" select="map {$root: ()}"/>
      <xsl:on-completion>
        <xsl:for-each select="map:keys($at)[not(. = $all)][1]">
          <xsl:sequence select="eo:taint(concat('The symbol ', ., ' of the entry ', $number, ' is read, while nothing defines it'))"/>
        </xsl:for-each>
        <xsl:sequence select="$at"/>
      </xsl:on-completion>
      <xsl:variable name="symbol" select="."/>
      <xsl:variable name="here" select="$at($symbol)"/>
      <xsl:variable name="joined" select="key('eo:joined', $symbol, $eo:doc)[1]"/>
      <xsl:variable name="reads" as="map(xs:string, xs:string*)*">
        <xsl:choose>
          <xsl:when test="not(map:contains($at, $symbol))"/>
          <xsl:when test="exists($joined)">
            <xsl:sequence select="(map {eo:condition($joined): $here}, map {eo:branch($joined, 1): ($here, concat($symbol, '+'))}, map {eo:branch($joined, 2): ($here, concat($symbol, '-'))})"/>
          </xsl:when>
          <xsl:when test="exists(key('eo:deferred', $symbol, $eo:doc))">
            <xsl:sequence select="eo:arguments(key('eo:deferred', $symbol, $eo:doc)[1])[not(map:contains($eo:voids, substring-after(., ':')))] ! map {substring-after(., ':'): $here}"/>
          </xsl:when>
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
        <xsl:with-param name="at" select="fold-left($symbols, $at, function($m, $r) { let $k := map:keys($r) return map:put($m, $k, if (map:contains($m, $k)) then eo:common($m($k), $r($k)) else $r($k)) })"/>
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
  <!-- A token is a constant when it is bytes written out, or a known symbol. -->
  <xsl:function name="eo:constant" as="xs:boolean">
    <xsl:param name="token" as="xs:string"/>
    <xsl:sequence select="not(starts-with($token, '𝜎')) or exists(key('eo:known', $token, $eo:doc))"/>
  </xsl:function>
  <!-- The bytes of a constant token. -->
  <xsl:function name="eo:bytes" as="xs:string">
    <xsl:param name="token" as="xs:string"/>
    <xsl:sequence select="if (starts-with($token, '𝜎')) then normalize-space(key('eo:known', $token, $eo:doc)[1]) else $token"/>
  </xsl:function>
  <!-- The symbols a minted symbol was made of, in the order of its operands. -->
  <xsl:function name="eo:operands" as="xs:string*">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:sequence select="tokenize(normalize-space(key('eo:minted', $symbol, $eo:doc)[1]), ' ')"/>
  </xsl:function>
  <!-- The symbol the fork of a joined symbol forks on. -->
  <xsl:function name="eo:condition" as="xs:string">
    <xsl:param name="joined" as="element()"/>
    <xsl:variable name="dataize" select="$joined/../dataize[starts-with(@meta, '𝛿1.')][1]"/>
    <xsl:if test="empty($dataize)">
      <xsl:sequence select="eo:taint(concat('The joined symbol ', $joined/@symbol, ' of the entry ', $number, ' has no condition'))"/>
    </xsl:if>
    <xsl:sequence select="substring-before(concat(string($dataize), ':'), ':')"/>
  </xsl:function>
  <!-- One branch of a joined symbol, the left one first. -->
  <xsl:function name="eo:branch" as="xs:string">
    <xsl:param name="joined" as="element()"/>
    <xsl:param name="side" as="xs:integer"/>
    <xsl:variable name="branches" select="tokenize(normalize-space($joined), ' ')"/>
    <xsl:if test="count($branches) != 2">
      <xsl:sequence select="eo:taint(concat('The joined symbol ', $joined/@symbol, ' of the entry ', $number, ' has ', count($branches), ' branches'))"/>
    </xsl:if>
    <xsl:sequence select="$branches[$side]"/>
  </xsl:function>
  <!--
  The arguments a deferred copy takes, each as its name and its symbol with
  a colon between them. A copy with an argument that is no symbol cannot be
  made, since the atom has nothing to bind that argument to, and a void left
  unset fails the copy once the program runs.
  -->
  <xsl:function name="eo:arguments" as="xs:string*">
    <xsl:param name="deferred" as="element()"/>
    <xsl:if test="empty($deferred/with)">
      <xsl:sequence select="eo:taint(concat('The deferred symbol ', $deferred/@symbol, ' of the entry ', $number, ' does not say which symbols its arguments are'))"/>
    </xsl:if>
    <xsl:variable name="arguments" select="$deferred/with/attr ! concat(@name, ':', normalize-space(.))"/>
    <xsl:for-each select="$arguments[not(starts-with(substring-after(., ':'), '𝜎'))][1]">
      <xsl:sequence select="eo:taint(concat('The deferred symbol ', $deferred/@symbol, ' of the entry ', $number, ' has the argument ', substring-before(., ':'), ', which is no symbol'))"/>
    </xsl:for-each>
    <xsl:sequence select="$arguments"/>
  </xsl:function>
  <!--
  The object a deferred copy is made of, as a chain of takes off the atom.
  It must be inside the formation of the entry, since the atom reaches
  nothing else, and outside its "φ", since the patching drops that "φ" along
  with all it holds.
  -->
  <xsl:function name="eo:copied" as="xs:string">
    <xsl:param name="deferred" as="element()"/>
    <xsl:variable name="of" select="string($deferred/@of)"/>
    <xsl:variable name="names" select="tokenize(substring-after($of, concat($locator, '.')), '\.')"/>
    <xsl:choose>
      <xsl:when test="$of = ''">
        <xsl:sequence select="eo:taint(concat('The deferred symbol ', $deferred/@symbol, ' of the entry ', $number, ' does not say which object it copies'))"/>
      </xsl:when>
      <xsl:when test="empty($names)">
        <xsl:sequence select="eo:taint(concat('The deferred symbol ', $deferred/@symbol, ' of the entry ', $number, ' copies ', $of, ', which is not inside ', $locator))"/>
      </xsl:when>
      <xsl:when test="$names[1] = 'φ'">
        <xsl:sequence select="eo:taint(concat('The deferred symbol ', $deferred/@symbol, ' of the entry ', $number, ' copies ', $of, ', which the atom replaces'))"/>
      </xsl:when>
      <xsl:when test="not(eo:data($of))">
        <xsl:sequence select="eo:taint(concat('The deferred symbol ', $deferred/@symbol, ' of the entry ', $number, ' copies ', $of, ', which inference does not say returns a number, a string, a bool or bytes'))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="string-join(('this.take(&quot;ρ&quot;)', $names ! concat('.take(&quot;', eo:literal(.), '&quot;)')), '')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  An object returns data when every type its body may be, as "eo:returns"
  reads it off the tables of "eo:inference", is a number, a string, a bool
  or bytes.
  -->
  <xsl:function name="eo:data" as="xs:boolean">
    <xsl:param name="loc" as="xs:string"/>
    <xsl:variable name="types" as="xs:string*" select="eo:returns($loc)"/>
    <xsl:sequence select="exists($types) and (every $t in $types satisfies $t = ('Φ.number', 'Φ.string', 'Φ.bool', 'Φ.true', 'Φ.false', 'Φ.bytes'))"/>
  </xsl:function>
  <!-- The Java of a symbol as an object a copy takes: a void as it is, anything else as data. -->
  <xsl:function name="eo:argument" as="xs:string">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:sequence select="if (map:contains($eo:voids, $symbol)) then eo:object($symbol) else concat('new Data.ToPhi(', if (eo:constant($symbol)) then eo:literal-of(eo:bytes($symbol), 'byte[]') else eo:local($symbol), ')')"/>
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
  operation wants one, which is where EO reads them as a number too.
  -->
  <xsl:function name="eo:type" as="xs:string">
    <xsl:param name="symbol" as="xs:string"/>
    <xsl:variable name="joined" select="key('eo:joined', $symbol, $eo:doc)[1]"/>
    <xsl:choose>
      <xsl:when test="map:contains($eo:voids, $symbol)">
        <xsl:sequence select="(map {'bool': 'boolean'}($eo:voids($symbol)[2]), 'byte[]')[1]"/>
      </xsl:when>
      <xsl:when test="exists(key('eo:deferred', $symbol, $eo:doc))">
        <xsl:sequence select="'byte[]'"/>
      </xsl:when>
      <xsl:when test="exists($joined)">
        <xsl:variable name="branches" select="(eo:branch($joined, 1), eo:branch($joined, 2))"/>
        <xsl:variable name="types" select="distinct-values($branches[not(eo:constant(.))] ! eo:type(.))"/>
        <xsl:choose>
          <xsl:when test="count($types) = 1">
            <xsl:sequence select="$types"/>
          </xsl:when>
          <xsl:when test="count($types) &gt; 1">
            <xsl:sequence select="eo:taint(concat('The branches of the joined symbol ', $symbol, ' of the entry ', $number, ' are ', string-join($types, ' and ')))"/>
          </xsl:when>
          <xsl:when test="every $b in $branches satisfies eo:bytes($b) = ('FF-', '00-')">
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
  <!-- The statements of the symbols placed at one place, in the order they were minted. -->
  <xsl:function name="eo:block" as="xs:string">
    <xsl:param name="at" as="map(xs:string, xs:string*)"/>
    <xsl:param name="place" as="xs:string*"/>
    <xsl:param name="depth" as="xs:integer"/>
    <xsl:sequence select="string-join(sort(map:keys($at)[deep-equal($at(.), $place)], (), eo:serial#1) ! eo:statement($at, ., $depth), '')"/>
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
        <xsl:sequence select="concat($indent, 'final ', $type, ' ', $local, ';&#10;', $indent, 'if (', eo:value(eo:condition($joined), 'boolean'), ') {&#10;', eo:block($at, ($place, concat($symbol, '+')), $depth + 1), $indent, '    ', $local, ' = ', eo:value(eo:branch($joined, 1), $type), ';&#10;', $indent, '} else {&#10;', eo:block($at, ($place, concat($symbol, '-')), $depth + 1), $indent, '    ', $local, ' = ', eo:value(eo:branch($joined, 2), $type), ';&#10;', $indent, '}&#10;')"/>
      </xsl:when>
      <xsl:when test="exists(key('eo:deferred', $symbol, $eo:doc))">
        <xsl:variable name="deferred" select="key('eo:deferred', $symbol, $eo:doc)[1]"/>
        <xsl:sequence select="concat($indent, 'final ', $type, ' ', $local, ' = new Dataized(new PhApplication(', string-join((eo:copied($deferred), eo:arguments($deferred) ! concat('new Bind(&quot;', eo:literal(substring-before(., ':')), '&quot;, ', eo:argument(substring-after(., ':')), ')')), ', '), ')).take();&#10;')"/>
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
