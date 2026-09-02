<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="to-eo-tree" version="2.0">
  <!-- An EO-source printer; its tree templates form one cohesive module. -->
  <!-- xslint-disable-file too-many-templates -->
  <!--
  This one maps XMIR to an intermediate "line tree" that is later
  laid out into pretty EO source by the Pretty class (penalty-based
  formatting). Each object becomes a <line> element carrying its
  rendered head ("base" attribute) and suffix ("tail" attribute),
  with nested <line> children for its (non-void) attributes. The
  layout engine decides, per node, whether to render it horizontally
  (inline) or vertically (indented), picking the option with the
  lowest penalty.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:variable name="eol" select="'&#10;'"/>
  <xsl:output method="xml" encoding="UTF-8"/>
  <!--
  What the "BASED" head template below used to re-derive for every single
  reference it renders, hoisted because none of it changes during one
  transformation (#7938). Saxon folds neither prefix nor the cactus pattern
  into a constant, "$eo:program" and "$eo:cactoos" being globals rather than
  literals, so "eo:printable" even recompiled its regex per call; and the
  "+package" meta, the program's own top-level name and the "+alias" short
  names were each read from the document root per reference.
  -->
  <xsl:variable name="eo:root-prefix" select="concat($eo:program, '.')"/>
  <xsl:variable name="eo:xi-prefix" select="concat($eo:xi, '.')"/>
  <xsl:variable name="eo:cactus-name" select="concat('a', $eo:cactoos)"/>
  <xsl:variable name="eo:cactus-pattern" select="concat('a', $eo:cactoos, '(\d+)-(\d+)')"/>
  <xsl:variable name="eo:package" select="string((/object/metas/meta[head='package'])[1]/part[1])"/>
  <xsl:variable name="eo:self-prefix" select="concat($eo:program, '.', $eo:package, '.')"/>
  <xsl:variable name="eo:top-name" as="xs:string*" select="/object/o[1]/@name/string()"/>
  <xsl:variable name="eo:aliases" as="xs:string*" select="/object/metas/meta[head = 'alias']/part[1]/string()"/>
  <!--
  The two scope questions that same template asks per reference, indexed: the
  handles of one name, reached through a "//o[@local=$name]" scan of the whole
  document, and whether an enclosing object declares a name, answered by
  scanning every ancestor's children. Both are the shape
  "resolve-local-names.xsl" keyed away in #6502, and both made this sheet
  quadratic in the file's size.
  -->
  <xsl:key name="locals" match="o[@local]" use="@local"/>
  <xsl:key name="named" match="o[@name]" use="concat(@name, '#', generate-id(..))"/>
  <!-- Translate a dotted path to EO surface form: ρ -> ^, φ -> @, ξ -> $. -->
  <xsl:function name="eo:translate-path" as="xs:string">
    <xsl:param name="path" as="xs:string"/>
    <xsl:sequence select="string-join(for $seg in tokenize($path, '\.') return (if ($seg = $eo:rho) then '^' else if ($seg = $eo:phi) then '@' else if ($seg = $eo:xi) then '$' else if ($seg = $eo:bottom) then 'T' else $seg), '.')"/>
  </xsl:function>
  <!--
  Render a base as an EO head: drop implicit ξ./Φ. roots, render a
  leading dot as reversed dispatch, map ξ/ρ/φ/Φ and every segment.

  The parameter takes the "@base" attribute node as it stands and atomises it
  here, rather than declaring "xs:string" and leaving that to the function
  conversion rules (#6669): Saxon may bind a parameter to a closure over the
  argument expression instead of over its converted value, and the node then
  reaches the "$base = $eo:xi" comparison below, which was compiled as a
  "ValueComparison" on the strength of a statically atomic parameter and casts
  straight to an atomic value. That is how the same shape killed
  "inline-cactoos" in #6669. "eo:root-name" is hardened the same way, being the
  other function this sheet asks about a raw "@base".
  -->
  <xsl:function name="eo:surface" as="xs:string">
    <xsl:param name="raw" as="item()?"/>
    <xsl:variable name="base" as="xs:string" select="string($raw)"/>
    <xsl:sequence select="if (starts-with($base, '.')) then concat(eo:translate-path(substring($base, 2)), '.') else if (starts-with($base, $eo:root-prefix)) then eo:translate-path(substring-after($base, $eo:root-prefix)) else if (starts-with($base, $eo:xi-prefix)) then eo:translate-path(substring-after($base, $eo:xi-prefix)) else if ($base = $eo:xi) then '$' else if ($base = $eo:program) then 'Q' else eo:translate-path($base)"/>
  </xsl:function>
  <!--
  A surface head with its last dot turned into "?." when the dispatch it
  ends in is fragile (R-3.5 / §9.4): "foo.first" becomes "foo?.first" and
  the reversed "first." becomes "first?.". A surface with no dot at all
  (nothing to mark) passes through unchanged.
  -->
  <xsl:function name="eo:fragile-marked" as="xs:string">
    <xsl:param name="surface" as="xs:string"/>
    <xsl:param name="fragile" as="xs:boolean"/>
    <xsl:variable name="last" select="tokenize($surface, '\.')[last()]"/>
    <xsl:sequence select="if ($fragile and contains($surface, '.')) then concat(substring($surface, 1, string-length($surface) - string-length($last) - 1), '?.', $last) else $surface"/>
  </xsl:function>
  <!--
  First name segment of a program-rooted base (Φ.foo.bar -> foo). The raw
  "@base" node is atomised here for the reason spelled out on "eo:surface"
  above (#6669).
  -->
  <xsl:function name="eo:root-name" as="xs:string">
    <xsl:param name="raw" as="item()?"/>
    <xsl:variable name="base" as="xs:string" select="string($raw)"/>
    <xsl:variable name="rest" select="substring-after($base, $eo:root-prefix)"/>
    <xsl:sequence select="if (contains($rest, '.')) then substring-before($rest, '.') else $rest"/>
  </xsl:function>
  <!-- Whether some object this reference is nested inside declares "$name". -->
  <xsl:function name="eo:declared" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:param name="name" as="xs:string"/>
    <xsl:sequence select="some $scope in $o/ancestor::o satisfies exists(key('named', concat($name, '#', generate-id($scope)), root($o)))"/>
  </xsl:function>
  <!--
  Rewrite surviving cactus names (a🌵L-P) to a valid id (vL_P). Called
  twice per emitted line, and almost no head or tail holds a cactus name, so
  "contains" decides the common case without invoking the regex engine.
  -->
  <xsl:function name="eo:printable" as="xs:string">
    <xsl:param name="text" as="xs:string"/>
    <xsl:sequence select="if (contains($text, $eo:cactus-name)) then replace($text, $eo:cactus-pattern, 'v$1_$2') else $text"/>
  </xsl:function>
  <!--
  Render a stored signature (an atom's "/sig" or a void forma) as its EO
  surface form. A rooted "Φ.name" always keeps an explicit "Q." root, so
  every rooted signature reads uniformly and re-parses directly back to its
  "Φ." root instead of relying on a bare name being re-homed. Names with no
  "Φ." root pass through.
  -->
  <xsl:function name="eo:signature" as="xs:string">
    <xsl:param name="sig" as="xs:string"/>
    <xsl:variable name="rest" select="substring-after($sig, $eo:root-prefix)"/>
    <xsl:sequence select="if (starts-with($sig, $eo:root-prefix)) then concat('Q.', $rest) else $sig"/>
  </xsl:function>
  <!-- Count the leading run of consecutive ρ segments in a sequence. -->
  <xsl:function name="eo:rho-run" as="xs:integer">
    <xsl:param name="segments" as="xs:string*"/>
    <xsl:sequence select="if (empty($segments) or $segments[1] != $eo:rho) then 0 else 1 + eo:rho-run(subsequence($segments, 2))"/>
  </xsl:function>
  <!--
  Whether a void has to be printed as a "? &gt; name" body line instead of
  being folded into the "[…]" bracket head. Three shapes cannot live in
  the head: a "&gt;&gt; name" handle, whose anonymity a public bracket param
  would blow (§9.2, R-9.2.3, #5581); a "/type" or "/{type …}" annotation,
  which a bracket param cannot express (#5614); and every void of an
  atom, whose head must stay empty (R-3.4.10) so a typed void may be
  followed by an untyped one without the two swapping places (#6082).
  -->
  <xsl:function name="eo:vertical-void" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="eo:void($o) and (exists($o/@local) or exists($o/@type) or exists($o/@args) or eo:atom($o/..))"/>
  </xsl:function>
  <!--
  A void's type tail (R-3.4.8): " /type" for its own forma, " /{type …}"
  for the argument types of a callback branch, or the empty string when
  the void is untyped. Every forma is rendered like the atom's own "/sig"
  — the implicit Φ. root stripped, a type variable verbatim — and a
  trailing "?" marking a maybe-⊥ value survives the stripping.
  -->
  <xsl:function name="eo:void-type" as="xs:string">
    <xsl:param name="o" as="element()"/>
    <xsl:choose>
      <xsl:when test="exists($o/@type)">
        <xsl:sequence select="concat(' /', eo:signature(replace($o/@type, '\?$', '')), if (ends-with($o/@type, '?')) then '?' else '')"/>
      </xsl:when>
      <xsl:when test="exists($o/@args)">
        <xsl:sequence select="concat(' /{', string-join(for $t in tokenize($o/@args, ' ') return eo:signature($t), ' '), '}')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="''"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  Whether an object is an identity object (#6834) — an anonymous
  formation that binds one void and decorates that very void, the shape
  "x &gt; [x]" the "I" glyph spells. Nothing but the φ ever reads that
  void, so its spelling carries no meaning and every spelling of it
  prints as the same one glyph. A φ that is applied to something, that
  is const, or that reaches any other name keeps the ordinary formation
  layout, since "I" cannot spell it.

  The two tests that look over-tight are load-bearing. Exactly two
  children: a third one is an argument applied to the formation, and the
  glyph leaves nowhere to put it, so such an object must keep the long
  layout. And the φ base rooted at "ξ": an unrooted name denotes some
  other object entirely, not the void beside it.
  -->
  <xsl:function name="eo:identity" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="eo:abstract($o) and not(eo:has-data($o)) and count($o/o) = 2 and eo:void($o/o[1]) and empty($o/o[1]/(@local, @type, @args)) and $o/o[2]/@name = $eo:phi and empty($o/o[2]/o) and empty($o/o[2]/@const) and $o/o[2]/@base = concat($eo:xi-prefix, $o/o[1]/@name)"/>
  </xsl:function>
  <!-- PROGRAM -->
  <xsl:template match="object">
    <object>
      <eo>
        <preamble>
          <xsl:apply-templates select="comments"/>
          <xsl:apply-templates select="license"/>
          <xsl:apply-templates select="metas"/>
        </preamble>
        <xsl:apply-templates select="o[1]" mode="tree"/>
      </eo>
    </object>
  </xsl:template>
  <!-- TOP COMMENT BLOCK -->
  <xsl:template match="comments">
    <xsl:for-each select="comment">
      <xsl:for-each select="tokenize(., $eol)">
        <xsl:choose>
          <xsl:when test=". = ''">
            <xsl:text>#</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text># </xsl:text>
            <xsl:value-of select="."/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:value-of select="$eol"/>
      </xsl:for-each>
    </xsl:for-each>
    <xsl:if test="comment">
      <xsl:value-of select="$eol"/>
    </xsl:if>
  </xsl:template>
  <!-- LICENCE -->
  <xsl:template match="license">
    <xsl:for-each select="tokenize(., $eol)">
      <xsl:text># </xsl:text>
      <xsl:value-of select="."/>
      <xsl:value-of select="$eol"/>
    </xsl:for-each>
    <xsl:if test="text()">
      <xsl:value-of select="$eol"/>
    </xsl:if>
  </xsl:template>
  <!-- METAS -->
  <xsl:template match="metas">
    <xsl:apply-templates select="meta"/>
    <xsl:value-of select="$eol"/>
  </xsl:template>
  <!-- META -->
  <xsl:template match="meta">
    <xsl:text>+</xsl:text>
    <xsl:value-of select="head"/>
    <!--
    An alias is stored as two parts: a local name and the fully
    qualified name (Φ.a.b.c). When the name is just the last dotted
    segment of the FQN it carries no information, so the idiomatic
    short form drops it and prints only the FQN with its Φ. root
    stripped (+alias a.b.c). A genuine rename, where the name
    differs from that last segment, keeps the two-argument form.
    -->
    <xsl:variable name="fqn" select="replace(part[last()], concat('^', $eo:program, '\.'), '')"/>
    <xsl:choose>
      <xsl:when test="head = 'alias' and count(part) = 2 and part[1] = tokenize($fqn, '\.')[last()]">
        <xsl:text> </xsl:text>
        <xsl:value-of select="$fqn"/>
      </xsl:when>
      <xsl:when test="not(empty(tail/text()))">
        <xsl:text> </xsl:text>
        <xsl:value-of select="replace(string(tail), $eo:program, 'Q')"/>
      </xsl:when>
      <xsl:otherwise/>
    </xsl:choose>
    <xsl:value-of select="$eol"/>
  </xsl:template>
  <!-- OBJECT, NOT FREE ATTRIBUTE -->
  <xsl:template match="o[not(eo:void(.)) and not(@name=$eo:lambda)]" mode="tree">
    <line>
      <xsl:variable name="head">
        <xsl:apply-templates select="." mode="head"/>
      </xsl:variable>
      <xsl:variable name="suffix">
        <xsl:apply-templates select="." mode="tail"/>
      </xsl:variable>
      <xsl:attribute name="base" select="eo:printable(string($head))"/>
      <xsl:attribute name="tail" select="eo:printable(string($suffix))"/>
      <xsl:attribute name="abstract">
        <xsl:value-of select="if (eo:abstract(.) and not(eo:has-data(.))) then 'yes' else 'no'"/>
      </xsl:attribute>
      <xsl:attribute name="test">
        <xsl:value-of select="if (eo:test-attr(.)) then 'yes' else 'no'"/>
      </xsl:attribute>
      <xsl:attribute name="reversed">
        <xsl:value-of select="if (@base and starts-with(@base, '.')) then 'yes' else 'no'"/>
      </xsl:attribute>
      <xsl:attribute name="data">
        <xsl:value-of select="if (eo:has-data(.)) then 'yes' else 'no'"/>
      </xsl:attribute>
      <!--
      Formation attributes are emitted in a canonical order so that
      textually-reordered but logically-identical bodies print the same
      way (idempotent, diff-friendly output — #5706). The ordering is:
      vertical void body lines first, then the decoratee (@name = φ),
      then every other bound attribute alphabetically by name, and test
      attributes last (also alphabetically). Two kinds of body keep
      their source order untouched: application arguments (positional,
      so their order carries meaning) and any body carrying a pipe
      continuation ("| args", §3.14), whose "|" line must stay directly
      below the named formation it applies to (R-3.14.7) — reordering
      would strand it. In both cases the sort key collapses to a
      constant, and since xsl:sort is stable the original order stands.
      -->
      <xsl:variable name="sortable" select="eo:abstract(.) and empty(o[@pipe])"/>
      <xsl:apply-templates select="o[not(eo:void(.)) or eo:vertical-void(.)]" mode="tree">
        <xsl:sort data-type="number" select="if (not($sortable)) then 0 else if (eo:void(.)) then 1 else if (@name = $eo:phi) then 2 else if (eo:test-attr(.)) then 4 else 3"/>
        <xsl:sort select="if (not($sortable) or eo:void(.) or @name = $eo:phi) then '' else string((@local, @name)[1])"/>
      </xsl:apply-templates>
    </line>
  </xsl:template>
  <!-- IDENTITY OBJECT -->
  <!--
  An identity object is a leaf on the line tree: its void and its φ are
  both spelled by the "I" glyph itself, so neither becomes a body line
  and the node lays out exactly like a bare "T" — inlined as an
  argument wherever the penalty allows it.
  -->
  <xsl:template match="o[eo:identity(.)]" mode="tree" priority="2">
    <xsl:variable name="suffix">
      <xsl:apply-templates select="." mode="tail"/>
    </xsl:variable>
    <line abstract="no" data="no" reversed="no">
      <xsl:attribute name="base">
        <xsl:apply-templates select="." mode="head"/>
      </xsl:attribute>
      <xsl:attribute name="tail" select="eo:printable(string($suffix))"/>
      <xsl:attribute name="test">
        <xsl:value-of select="if (eo:test-attr(.)) then 'yes' else 'no'"/>
      </xsl:attribute>
    </line>
  </xsl:template>
  <xsl:template match="o[eo:identity(.)]" mode="head" priority="2">
    <xsl:text>I</xsl:text>
  </xsl:template>
  <!-- VOID AS A VERTICAL BODY LINE -->
  <!--
  A void the bracket head cannot hold, printed as a "? &gt; name" body line
  (R-3.4.7); "eo:vertical-void" names the three shapes. A "&gt;&gt; name"
  handle survives "restore-local-names" in @local and keeps the void
  anonymous; a "/type" or "/{type …}" tail is rendered by
  "eo:void-type"; and a φ or ρ void reverts to its "@" or "^" surface
  spelling.
  -->
  <xsl:template match="o[eo:vertical-void(.)]" mode="tree">
    <xsl:variable name="arrow" select="if (exists(@local)) then ' &gt;&gt; ' else ' &gt; '"/>
    <xsl:variable name="label" select="if (exists(@local)) then string(@local) else if (@name = $eo:phi) then '@' else if (@name = $eo:rho) then '^' else string(@name)"/>
    <line base="?" tail="{concat($arrow, $label, eo:void-type(.))}" abstract="no" test="no" reversed="no"/>
  </xsl:template>
  <!-- PIPE APPLICATION (§3.14) -->
  <!--
  A "| args &gt; name" continuation line (R-3.14.7). The parser rewrote
  it into an application of its same-indent predecessor, pointing @base
  at that predecessor's name but keeping the @pipe marker (#5684). We
  restore the compact "|" head only when that predecessor is still the
  immediately preceding sibling, so the emitted pipe has a valid target
  directly above it — the base then reads "ξ.&lt;name&gt;" (or the bare
  "&lt;name&gt;" if reference resolution has not rooted it). When the
  predecessor has floated away (#5526) the guard fails and the node
  prints as an ordinary application, which round-trips just as safely.
  -->
  <xsl:template match="o[@pipe and (@base = concat($eo:xi-prefix, preceding-sibling::o[1]/@name) or @base = preceding-sibling::o[1]/@name)]" mode="head" priority="2">
    <xsl:text>|</xsl:text>
  </xsl:template>
  <!-- BASED -->
  <xsl:template match="o[@base and not(eo:has-data(.))]" mode="head">
    <!--
    A base of the form "ξ.ρ…ρ.name…" is a run of parent hops onto a
    name that lives in an enclosing scope. "rho-count" counts the
    consecutive leading "ρ" segments after the "ξ." root, "hop-name" is
    the first segment past that run and "hop-rest" is that segment plus
    any trailing segments.
    -->
    <xsl:variable name="segments" select="tokenize(@base, '\.')"/>
    <xsl:variable name="rho-count" select="if ($segments[1] = $eo:xi) then eo:rho-run(subsequence($segments, 2)) else 0"/>
    <xsl:variable name="hop-name" select="if (count($segments) &gt; $rho-count + 1) then $segments[$rho-count + 2] else ''"/>
    <xsl:variable name="hop-rest" select="string-join(subsequence($segments, $rho-count + 2), '.')"/>
    <!--
    What the "Φ.<package>." prefix a self-reference to a same-file object
    carries (see "eo:self-prefix", built from the "+package" meta at the head
    of this sheet) leaves behind, once stripped.
    -->
    <xsl:variable name="self-rest" select="substring-after(@base, $eo:self-prefix)"/>
    <xsl:variable name="self-first" select="if (contains($self-rest, '.')) then substring-before($self-rest, '.') else $self-rest"/>
    <!-- The first name segment of a program-rooted base, asked about twice below. -->
    <xsl:variable name="root-name" as="xs:string" select="eo:root-name(@base)"/>
    <!--
    Whether some formation this reference is actually nested inside (not
    just the innermost one - a zero-hop reference here can legitimately
    reach a handle owned by any enclosing formation, not only the
    nearest, since resolve-local-names.xsl already resolved it) owns
    "hop-name" as its own file-local "&gt;&gt; hop-name" handle - mirrors
    resolve-local-names.xsl's own "eo:captor" scoping (#5875, #5780): a
    handle is lexically scoped to its declaring formation and the
    formations nested inside it, so a same-named handle declared by some
    unrelated formation elsewhere in the file (not an ancestor of this
    reference at all) must not suppress the "$." marker here.
    -->
    <xsl:variable name="local-handle" select="key('locals', $hop-name)[ancestor::o[not(@base)][1] intersect current()/ancestor::o[not(@base)]][1]"/>
    <!--
    The surface the arms below build, before its fragile marker is put
    back. A dispatch stays fragile whatever shortening its base happens
    to qualify for, so the marker goes back on the last dot of whatever
    surface the choose produced (#6912); dotless heads ("*", "T", "$",
    "Q") have no dispatch to mark and pass through untouched.
    -->
    <xsl:variable name="surface" as="xs:string">
      <xsl:choose>
        <!-- NOT OPTIMIZED TUPLE -->
        <xsl:when test="@star">
          <xsl:text>*</xsl:text>
        </xsl:when>
        <xsl:when test="@base=$eo:bottom">
          <xsl:text>T</xsl:text>
        </xsl:when>
        <xsl:when test="starts-with(@base, $eo:root-prefix) and (eo:declared(., $root-name) or $eo:aliases = $root-name)">
          <!--
          The plain top-level name would be shadowed by an in-scope
          attribute, or reinterpreted through a "+alias" declaring that
          same short name for a different object (#6211), so keep the
          explicit Q. root to disambiguate.
          -->
          <xsl:value-of select="concat('Q.', eo:translate-path(substring-after(@base, $eo:root-prefix)))"/>
        </xsl:when>
        <xsl:when test="$eo:package != '' and starts-with(@base, $eo:self-prefix) and $self-first = $eo:top-name and not(eo:declared(., $self-first))">
          <!--
          The base names this program's own top-level object through its
          fully-qualified "Φ.<package>.<name>…" form. The source wrote it
          bare and every other same-package reference prints bare, so drop
          the redundant "Φ.<package>." prefix and render the bare name. If
          an in-scope attribute shadows that name, this branch is skipped:
          either the package segment is shadowed too (kept "Q."-rooted by
          the branch above) or the qualified "<package>.<name>" survives
          through the otherwise branch, both of which resolve correctly.
          -->
          <xsl:value-of select="eo:translate-path($self-rest)"/>
        </xsl:when>
        <xsl:when test="not(@local) and $segments[1] = $eo:xi and $rho-count = 0 and $hop-name != '' and $hop-name != $eo:rho and $hop-name != $eo:phi and $hop-name != $eo:xi and $hop-name != $eo:program and empty(ancestor::o[not(@base)][1]/o[@name=$hop-name]) and empty($local-handle)">
          <!--
          An explicit "$.name" (zero ρ hops) whose immediate enclosing
          formation has no "name" of its own, and which is not a
          ">> name" handle owned by a formation this reference actually
          nests inside either ("local-handle" mirrors resolve-local-names.xsl's
          lexically-scoped "eo:captor", #5875 — a same-named handle declared
          by some unrelated formation elsewhere in the file, one this
          reference is not nested inside, must not count), denotes a
          genuinely different reference than a bare "name" would (which
          would instead resolve via an implicit ρ hop into an outer scope,
          per the branch above) — #6237. Render it with its "$." marker so
          reparsing keeps the same meaning. When the immediate formation
          DOES own "name", or "name" is such an in-scope file-local handle,
          bare and "$.name" resolve identically, so the otherwise branch
          below safely prints it bare, matching a plain same-scope
          reference. A node that itself carries "@local" is a moniker's own
          bound value, already relocated in place of its reference site by
          merge-monikers.xsl, so its ancestor context here no longer
          reflects its original scope — skip it too, matching bare.
          -->
          <xsl:value-of select="concat('$.', eo:translate-path($hop-rest))"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="eo:surface(@base)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="eo:fragile-marked($surface, exists(@fragile))"/>
  </xsl:template>
  <!-- ABSTRACT OR ATOM -->
  <xsl:template match="o[eo:abstract(.) and not(eo:has-data(.))]" mode="head">
    <!--
    A test attribute with no void params collapses its empty `[]`
    head into the `++&gt;` suffix (see the tail template), so it emits
    no head of its own.
    -->
    <xsl:if test="not(eo:test-attr(.) and empty(o[eo:void(.)]))">
      <xsl:text>[</xsl:text>
      <xsl:for-each select="o[eo:void(.) and not(eo:vertical-void(.))]">
        <xsl:if test="position()&gt;1">
          <xsl:text> </xsl:text>
        </xsl:if>
        <xsl:choose>
          <xsl:when test="@name = $eo:phi">
            <xsl:value-of select="'@'"/>
          </xsl:when>
          <xsl:when test="@name = $eo:rho">
            <xsl:value-of select="'^'"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="@name"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
      <xsl:text>]</xsl:text>
    </xsl:if>
  </xsl:template>
  <!-- TAIL: SUFFIX, NAME, CONST, ATOM -->
  <xsl:template match="o" mode="tail">
    <!--
    "@as" labels the void this object fills when it stands as an
    argument (R-3.12). The same node keeps that stale "@as" when it is
    also bound to a real, written-out name and printed as its own body
    line, "then.baz > z" (#7453): a label only means something at the
    call site, so it is dropped whenever "@name" shows this node is
    defined there instead - unless "@name" is only the auto-generated
    cactus placeholder of an anonymous inline handle ("[f]:1 &gt;&gt;"),
    which is itself the argument the label belongs to, not a separate
    definition.
    -->
    <xsl:if test="@as and (not(@name) or starts-with(@name, $eo:cactus-name))">
      <xsl:text>:</xsl:text>
      <xsl:choose>
        <xsl:when test="starts-with(@as, $eo:alpha)">
          <xsl:value-of select="substring-after(@as, $eo:alpha)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="eo:translate-path(@as)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:if test="@name">
      <xsl:choose>
        <xsl:when test="eo:test-attr(.)">
          <!--
          The marker char is a plus for a truthy test (`+name`) or a
          minus for a throwing test (`-name`); it doubles into the
          head-of-line shorthand and stays single for the mid-line
          suffix.
          -->
          <xsl:variable name="marker" select="substring(@name, 1, 1)"/>
          <xsl:choose>
            <!--
            An abstract formation with no void params: collapse its
            empty `[]` head into the single doubled-marker head-of-line
            shorthand (the head template emits nothing in this case).
            A non-abstract object (a plain reference or application,
            "true +&gt; works") has a rendered head of its own, so the
            doubled marker would glue onto it instead (#7451); that
            case falls through to the single-marker branch below.
            -->
            <xsl:when test="eo:abstract(.) and empty(o[eo:void(.)])">
              <xsl:value-of select="$marker"/>
              <xsl:value-of select="$marker"/>
              <xsl:text>&gt; </xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text> </xsl:text>
              <xsl:value-of select="$marker"/>
              <xsl:text>&gt; </xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:value-of select="substring(@name, 2)"/>
        </xsl:when>
        <xsl:when test="@local">
          <!--
          A non-void formation declared with a file-local handle
          ("&gt;&gt; name", R-3.10.12): a recursive helper kept in place by
          "inline-cactoos" (#5677) whose "@local" marker "restore-local-names"
          preserves (#5681). It is printed with its readable handle under the
          double-arrow, not as an anonymous "&gt;&gt;".
          -->
          <xsl:text> &gt;&gt; </xsl:text>
          <xsl:value-of select="@local"/>
        </xsl:when>
        <xsl:when test="starts-with(@name, $eo:cactus-name)">
          <xsl:text> &gt;&gt;</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text> &gt; </xsl:text>
          <xsl:choose>
            <xsl:when test="@name = $eo:phi">
              <xsl:value-of select="'@'"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="@name"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="eo:atom(.)">
        <!--
        A multi-segment signature (e.g. "Φ.string.regex.pattern") stays
        rooted: on reparse a dotted @atom is not re-homed, so dropping the
        root would yield an XSD-invalid fqn. A single-name signature (e.g.
        "Φ.bytes") re-resolves to its root on reparse, so the implicit
        root is dropped for idiomatic output.
        -->
        <xsl:value-of select="concat(' /', eo:signature(string(./o[@name=$eo:lambda]/@atom)))"/>
      </xsl:if>
    </xsl:if>
    <!--
    The `!` const marker, for a named binding (`... > name!`, `[] >>!`)
    and for an anonymous inline const argument (`42.plus a!`, #5821) that
    carries `@const` with no `@name`.
    -->
    <xsl:if test="@const">
      <xsl:text>!</xsl:text>
    </xsl:if>
  </xsl:template>
  <!-- DATA -->
  <xsl:template match="o[eo:has-data(.)]" mode="head">
    <xsl:value-of select="eo:read-data(.)"/>
  </xsl:template>
</xsl:stylesheet>
