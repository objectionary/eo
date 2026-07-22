<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="to-eo-tree" version="2.0">
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
  <!-- Translate a dotted path to EO surface form: ρ -> ^, φ -> @. -->
  <xsl:function name="eo:translate-path" as="xs:string">
    <xsl:param name="path" as="xs:string"/>
    <xsl:sequence select="string-join(for $seg in tokenize($path, '\.') return (if ($seg = $eo:rho) then '^' else if ($seg = $eo:phi) then '@' else $seg), '.')"/>
  </xsl:function>
  <!--
  Render a base as an EO head: drop implicit ξ./Φ. roots, render a
  leading dot as reversed dispatch, map ξ/ρ/φ/Φ and every segment.
  -->
  <xsl:function name="eo:surface" as="xs:string">
    <xsl:param name="base" as="xs:string"/>
    <xsl:sequence select="if (starts-with($base, '.')) then concat(eo:translate-path(substring($base, 2)), '.') else if (starts-with($base, concat($eo:program, '.'))) then eo:translate-path(substring-after($base, concat($eo:program, '.'))) else if (starts-with($base, concat($eo:xi, '.'))) then eo:translate-path(substring-after($base, concat($eo:xi, '.'))) else if ($base = $eo:xi) then '$' else if ($base = $eo:program) then 'Q' else eo:translate-path($base)"/>
  </xsl:function>
  <!-- First name segment of a program-rooted base (Φ.foo.bar -> foo). -->
  <xsl:function name="eo:root-name" as="xs:string">
    <xsl:param name="base" as="xs:string"/>
    <xsl:variable name="rest" select="substring-after($base, concat($eo:program, '.'))"/>
    <xsl:sequence select="if (contains($rest, '.')) then substring-before($rest, '.') else $rest"/>
  </xsl:function>
  <!-- Rewrite surviving cactus names (a🌵L-P) to a valid id (vL_P). -->
  <xsl:function name="eo:printable" as="xs:string">
    <xsl:param name="text" as="xs:string"/>
    <xsl:sequence select="replace($text, concat('a', $eo:cactoos, '(\d+)-(\d+)'), 'v$1_$2')"/>
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
    <xsl:variable name="root" select="concat($eo:program, '.')"/>
    <xsl:variable name="rest" select="substring-after($sig, $root)"/>
    <xsl:sequence select="if (starts-with($sig, $root)) then concat('Q.', $rest) else $sig"/>
  </xsl:function>
  <!-- Count the leading run of consecutive ρ segments in a sequence. -->
  <xsl:function name="eo:rho-run" as="xs:integer">
    <xsl:param name="segments" as="xs:string*"/>
    <xsl:sequence select="if (empty($segments) or $segments[1] != $eo:rho) then 0 else 1 + eo:rho-run(subsequence($segments, 2))"/>
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
      <xsl:apply-templates select="o[not(eo:void(.)) or @local or @types]" mode="tree">
        <xsl:sort data-type="number" select="if (not($sortable)) then 0 else if (eo:void(.)) then 1 else if (@name = $eo:phi) then 2 else if (eo:test-attr(.)) then 4 else 3"/>
        <xsl:sort select="if (not($sortable) or eo:void(.) or @name = $eo:phi) then '' else string(@name)"/>
      </xsl:apply-templates>
    </line>
  </xsl:template>
  <!-- VOID WITH FILE-LOCAL HANDLE -->
  <!--
  A void declared "? &gt;&gt; name" (R-3.10.12) keeps its @local handle
  through "restore-local-names"; it is printed as a vertical body line
  to preserve the void's anonymity (§9.2, R-9.2.3) instead of being
  collapsed into a public "[name]" bracket param (#5581).
  -->
  <xsl:template match="o[eo:void(.) and @local and not(@types)]" mode="tree">
    <line base="?" tail="{concat(' &gt;&gt; ', @local)}" abstract="no" test="no" reversed="no"/>
  </xsl:template>
  <!-- VOID WITH TYPE ANNOTATION -->
  <!--
  A void carrying a forma-list tail ("? &gt; name /{forma …}", R-3.4.8) —
  the argument formas of an atom's error branch — is printed as a
  vertical body line so its "/{...}" annotation survives. A bracket param
  cannot express a type, so folding it into "[...]" would silently drop
  the signature and change the object's shape while still parsing (#5614).
  Each forma has its implicit Φ. root stripped, mirroring the atom's own
  "/sig" rendering.
  -->
  <xsl:template match="o[eo:void(.) and @types]" mode="tree">
    <xsl:variable name="formas" select="string-join(for $t in tokenize(@types, ' ') return eo:signature($t), ' ')"/>
    <xsl:choose>
      <xsl:when test="@local">
        <line base="?" tail="{concat(' &gt;&gt; ', @local, ' /{', $formas, '}')}" abstract="no" test="no" reversed="no"/>
      </xsl:when>
      <xsl:otherwise>
        <line base="?" tail="{concat(' &gt; ', @name, ' /{', $formas, '}')}" abstract="no" test="no" reversed="no"/>
      </xsl:otherwise>
    </xsl:choose>
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
  <xsl:template match="o[@pipe and (@base = concat($eo:xi, '.', preceding-sibling::o[1]/@name) or @base = preceding-sibling::o[1]/@name)]" mode="head" priority="2">
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
    <xsl:variable name="rho-prefix" select="concat($eo:xi, '.', $eo:rho, '.')"/>
    <xsl:variable name="segments" select="tokenize(@base, '\.')"/>
    <xsl:variable name="rho-count" select="if ($segments[1] = $eo:xi) then eo:rho-run(subsequence($segments, 2)) else 0"/>
    <xsl:variable name="hop-name" select="if (count($segments) &gt; $rho-count + 1) then $segments[$rho-count + 2] else ''"/>
    <xsl:variable name="hop-rest" select="string-join(subsequence($segments, $rho-count + 2), '.')"/>
    <!--
    The current program's "+package" and the "Φ.<package>." prefix a
    self-reference to a same-file object carries after being homed
    into the package (add-default-package / build-fqns).
    -->
    <xsl:variable name="package" select="string(/object/metas/meta[head='package']/part[1])"/>
    <xsl:variable name="self-prefix" select="concat($eo:program, '.', $package, '.')"/>
    <xsl:variable name="self-rest" select="substring-after(@base, $self-prefix)"/>
    <xsl:variable name="self-first" select="if (contains($self-rest, '.')) then substring-before($self-rest, '.') else $self-rest"/>
    <xsl:choose>
      <!-- NOT OPTIMIZED TUPLE -->
      <xsl:when test="@star">
        <xsl:text>*</xsl:text>
      </xsl:when>
      <xsl:when test="@base=$eo:bottom">
        <xsl:text>T</xsl:text>
      </xsl:when>
      <xsl:when test="starts-with(@base, concat($eo:program, '.')) and exists(ancestor::o/o[@name = eo:root-name(current()/@base)])">
        <!--
        The plain top-level name would be shadowed by an in-scope
        attribute, so keep the explicit Q. root to disambiguate.
        -->
        <xsl:value-of select="concat('Q.', eo:translate-path(substring-after(@base, concat($eo:program, '.'))))"/>
      </xsl:when>
      <xsl:when test="$package != '' and starts-with(@base, $self-prefix) and $self-first = /object/o[1]/@name and empty(ancestor::o/o[@name = $self-first])">
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
      <xsl:when test="starts-with(@base, $rho-prefix) and $hop-name != '' and (every $i in 1 to $rho-count - 1 satisfies ancestor::o[not(@base)][$i]/@local != $hop-name) and ancestor::o[not(@base)][$rho-count]/@local = $hop-name">
        <!--
        The ρ-run lands on the file-local "&gt;&gt; name" handle of an
        enclosing formation, i.e. a recursive self-reference inlined as a
        moniker. The handle is in scope from inside its own body, so plain
        resolution of the bare name re-derives the same ρ-chain; drop the
        run and print the handle.
        -->
        <xsl:value-of select="eo:translate-path($hop-rest)"/>
      </xsl:when>
      <xsl:when test="starts-with(@base, $rho-prefix) and $hop-name != '' and $hop-name != $eo:rho and $hop-name != $eo:phi and $hop-name != $eo:xi and $hop-name != $eo:program and (every $i in 1 to $rho-count satisfies empty(ancestor::o[not(@base)][$i]/o[@name=$hop-name])) and exists(ancestor::o[not(@base)][$rho-count + 1]/o[@name=$hop-name])">
        <!--
        The run of leading "^." parent hops is redundant: the name is
        absent from each of the N nearer enclosing formations but
        present in the (N+1)-th, so plain scope resolution re-derives
        exactly the very same ρ-chain of length N. Drop the whole run.
        -->
        <xsl:value-of select="eo:translate-path($hop-rest)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="eo:surface(@base)"/>
      </xsl:otherwise>
    </xsl:choose>
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
      <xsl:for-each select="o[eo:void(.) and not(@local) and not(@types)]">
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
    <xsl:if test="@as">
      <xsl:text>:</xsl:text>
      <xsl:choose>
        <xsl:when test="starts-with(@as, $eo:alpha)">
          <xsl:value-of select="substring-after(@as, $eo:alpha)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@as"/>
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
            No void params: collapse the empty `[]` head into the
            single doubled-marker head-of-line shorthand (the head
            template emits nothing in this case).
            -->
            <xsl:when test="empty(o[eo:void(.)])">
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
        <xsl:when test="starts-with(@name, concat('a', $eo:cactoos))">
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
      <xsl:if test="@const">
        <xsl:text>!</xsl:text>
      </xsl:if>
      <xsl:if test="eo:atom(.)">
        <!--
        A multi-segment signature (e.g. "Φ.malloc.of.allocated") stays
        rooted: on reparse a dotted @atom is not re-homed, so dropping the
        root would yield an XSD-invalid fqn. A single-name signature (e.g.
        "Φ.bytes") re-resolves to its root on reparse, so the implicit
        root is dropped for idiomatic output.
        -->
        <xsl:value-of select="concat(' /', eo:signature(string(./o[@name=$eo:lambda]/@atom)))"/>
      </xsl:if>
    </xsl:if>
  </xsl:template>
  <!-- DATA -->
  <xsl:template match="o[eo:has-data(.)]" mode="head">
    <xsl:value-of select="eo:read-data(.)"/>
  </xsl:template>
</xsl:stylesheet>
