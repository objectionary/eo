<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="https://www.eolang.org" exclude-result-prefixes="xs eo" id="resolve-local-names" version="2.0">
  <xsl:import href="/org/eolang/parser/_specials.xsl"/>
  <!--
  The "&gt;&gt; foo" file-local handle (§3.10 / §9.2): the parser emits the
  anonymous object with its cactus @name plus a "@local='foo'" marker. A
  handle is an attribute of the formation that declares it, so we rewrite a
  @base equal to a handle name into that (reserved) cactus name; a handle
  declared more than once within the same enclosing formation is an error,
  but two sibling formations may each declare a same-named handle because
  resolution is lexically scoped (#5875). The "@local" marker is deliberately
  kept on the declaring object so that later passes (in particular the
  printer, see #5563) can recover the readable handle from the otherwise-
  synthetic cactus name instead of printing a placeholder like "vL_P".

  The rewrite is guarded by lexical scope (#5780). A reference binds to a
  handle only when the formation that declares the handle is the nearest
  enclosing scope of the reference that declares the name at all - so a
  nearer "&gt; foo" attribute (or "&gt;&gt; foo" handle) of the same name
  shadows a handle in a more distant scope, and a file-local handle no longer
  hijacks a same-named public attribute in an unrelated sibling formation.
  This mirrors how "build-fqns" later resolves a name to the nearest
  enclosing formation that owns an attribute of that name.

  A reference may also spell its scope out instead of leaving it to the
  search: "$.foo" reaches the innermost formation, "^.foo" its parent
  (§3.14), and "bar.foo" the enclosing formation named "bar", and each binds
  to the handle of exactly the formation it names (see `eo:scope`, #5917 and
  #5960). Such a reference is the only spelling left when
  the nested formation rebinds the name it reads - `^.foo &gt; foo` copies
  the parent's handle into an attribute of the same name, where a bare "foo"
  would bind to the copy - so leaving it unresolved stranded the handle
  behind a name nothing owned, and the reference dataized to ⊥.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  Every handle, indexed by its name together with the formation that owns
  it - the same "name in this scope" pair the search below asks about. The
  question "does this formation declare a handle of this name" is then a
  hash lookup, where it used to be a walk of the formation's entire subtree
  ("some $d in descendant::o[@local=$name] ..."), repeated for every
  ancestor of every reference. That walk made the pass quadratic in the size
  of the file, so a few tens of thousands of objects took tens of seconds
  (#6502).

  The pair is spelled as a string because "xsl:key" indexes by atomic value,
  and "generate-id" is the only way to name a node in one: two formations
  are the same scope exactly when their generated ids are equal. A handle
  written at the top level, outside any formation, has no owner and lands
  under the id-less "name#" bucket, which no formation ever asks for.
  -->
  <xsl:key name="handles" match="o[@local]" use="concat(@local, '#', generate-id(ancestor::o[not(@base)][1]))"/>
  <!--
  Every named object, indexed by its name together with its parent, so the
  "does this formation have an attribute of this name" test is a hash lookup
  as well, rather than a scan of the formation's children.
  -->
  <xsl:key name="attributes" match="o[@name]" use="concat(@name, '#', generate-id(..))"/>
  <!--
  The file-local handle that captures the given reference, or the empty
  sequence when the reference resolves to something else (a public attribute
  or a global). The nearest enclosing formation (an "o" without "@base") that
  declares the referenced name - as a public attribute "@name" or as a handle
  "@local" - is the reference's scope; the reference binds to a handle only
  when that scope's declaration is itself a handle.

  A handle is an attribute of the formation that declares it wherever it is
  written in that formation's body, not only as a direct child: a handle
  written nested inside an application (`42.plus > x` with `a &gt;&gt; b!`
  beneath it, the moniker spelling the printer emits, #5828) still belongs to
  the enclosing formation. So a formation owns a handle when some "@local"
  descendant has that formation as its nearest enclosing formation - which is
  exactly the owner the "handles" key files each handle under, so a handle
  never leaks up out of, or sideways between, nested formations (#5780).

  Both questions the search asks - "does this formation own an attribute of
  this name" and "does it own a handle of this name" - go through the keys
  above, so each ancestor costs a hash lookup instead of a subtree walk. The
  nearest such ancestor is taken with "[1]" on the (reverse) ancestor axis
  rather than "[last()]" on a parenthesized, document-ordered sequence: the
  two name the same formation, but the former lets the search stop at the
  first ancestor that declares the name, while the latter has to test every
  ancestor up to the root before it can tell which one came last.
  -->
  <xsl:function name="eo:captor" as="element()?">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="name" as="xs:string" select="if (exists($ref/@method)) then substring-after($ref/@base, '.') else string($ref/@base)"/>
    <xsl:variable name="scope" as="element()?" select="if (exists($ref/@method)) then eo:scope($ref) else $ref/ancestor::o[not(@base)][exists(key('attributes', concat($name, '#', generate-id(.)))) or exists(key('handles', concat($name, '#', generate-id(.))))][1]"/>
    <xsl:sequence select="for $found in $scope return key('handles', concat($name, '#', generate-id($found)), $found)[1]"/>
  </xsl:function>
  <!--
  The formation that the explicit receiver of a dispatch names: "ξ" (written
  "$") is the innermost formation, "ρ" ("^") is its parent, and every further
  ".ρ" segment climbs one level more. Such a receiver pins the scope instead
  of leaving it to the search above, so "^.foo" reaches exactly one formation
  out and stops there, however many scopes further out also declare the name.
  A formation may equally be named by its own name: "bar.foo" written inside
  "bar" reaches the same object "$.foo" does, since a name resolves to the
  nearest enclosing formation that owns it and a formation owns itself
  (#5960). The name is taken as the scope only when the nearest formation
  that declares it at all is the one carrying it, so a nearer attribute of
  the same name shadows the formation and pins nothing, as it does for a bare
  reference above.

  A dispatch is still flat at this point in the train - "wrap-method-calls"
  nests it later - so the receiver of a "@method" node is its preceding
  sibling; a reversed dispatch ("plus. &gt; y") carries no "@method" and holds
  its receiver as a child, so it never reaches here. The empty sequence for
  every other receiver (an application, a global, a name of no enclosing
  formation), which names no scope known at parse time.
  -->
  <xsl:function name="eo:scope" as="element()?">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="receiver" as="element()?" select="$ref/preceding-sibling::o[1]"/>
    <xsl:variable name="owner" as="element()?" select="$ref/ancestor::o[not(@base)][@name=$receiver/@base or exists(key('attributes', concat($receiver/@base, '#', generate-id(.))))][1]"/>
    <xsl:sequence select="if (empty($receiver/@base)) then () else if ($receiver/@base='ξ' and empty($receiver/@method)) then $ref/ancestor::o[not(@base)][1] else if ($receiver/@base='ρ' and empty($receiver/@method)) then $ref/ancestor::o[not(@base)][2] else if ($receiver/@base='.ρ' and exists($receiver/@method)) then eo:scope($receiver)/ancestor::o[not(@base)][1] else if (empty($receiver/@method) and $owner/@name=$receiver/@base) then $owner else ()"/>
  </xsl:function>
  <!--
  How many formations separate a bare, ancestor-search-captured reference
  from the formation that owns the handle capturing it - the same "rhos"
  count "build-fqns.xsl" computes for every other name, so the receiver
  built below matches what that stage would have built itself, and it never
  needs to walk scopes for a cactus name again (#7134).
  -->
  <xsl:function name="eo:hops" as="xs:integer">
    <xsl:param name="ref" as="element()"/>
    <xsl:param name="owner" as="element()"/>
    <xsl:sequence select="count($ref/ancestor::o[not(@base)]) - count($owner/ancestor-or-self::o[not(@base)])"/>
  </xsl:function>
  <!--
  The receiver a captured bare reference dispatches through: "ξ" itself
  when the handle lives in the reference's own formation, or that many
  ".ρ" hops out otherwise - the exact shape "build-fqns.xsl"'s "with-rho"
  builds from a "rhos" count, built here instead since this pass is the one
  that knows the count.
  -->
  <xsl:function name="eo:receiver" as="element()">
    <xsl:param name="hops" as="xs:integer"/>
    <xsl:choose>
      <xsl:when test="$hops le 0">
        <o>
          <xsl:attribute name="base" select="'ξ'"/>
        </o>
      </xsl:when>
      <xsl:otherwise>
        <o>
          <xsl:attribute name="base" select="'.ρ'"/>
          <xsl:sequence select="eo:receiver($hops - 1)"/>
        </o>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  Matches every reference and rewrites the ones a handle captures. The
  captor is looked up once, into a variable, instead of once in the match
  pattern and again in the body: the search is the expensive part of this
  pass, and a pattern that calls it cannot share its answer with the
  template it selects, so every captured reference paid for it twice.

  A reference captured through an explicit receiver ("^.foo" via
  "eo:scope") keeps that receiver as the author wrote it, so only its
  trailing name is rewritten; so does one captured in its own declaring
  formation (zero hops), which "build-fqns.xsl" already resolves correctly
  on its own, the same way it does for a bare public-attribute name found
  in the current scope. Only a bare reference captured one or more
  formations further out carries no receiver at all, so one is built here
  from the hop count, with the handle's path arriving already resolved
  instead of left for "build-fqns.xsl" to re-derive through its cactus
  exception.
  -->
  <xsl:template match="o[@base]">
    <xsl:variable name="captor" as="element()?" select="eo:captor(.)"/>
    <xsl:variable name="name" as="xs:string" select="string(@base)"/>
    <xsl:variable name="anonymous" as="element()?" select="if (exists($captor) or exists(@method) or not(contains($name, $eo:cactoos))) then () else ancestor::o[@name=$name][1]"/>
    <xsl:variable name="holder" as="element()?" select="($captor, $anonymous)[1]"/>
    <xsl:variable name="hops" as="xs:integer?" select="if (exists($holder) and empty(@method)) then eo:hops(., $holder/ancestor::o[not(@base)][1]) else ()"/>
    <xsl:choose>
      <xsl:when test="exists($hops) and $hops gt 0">
        <xsl:copy>
          <xsl:attribute name="base" select="concat('.', $holder/@name)"/>
          <xsl:apply-templates select="@* except @base"/>
          <xsl:sequence select="eo:receiver($hops)"/>
          <xsl:apply-templates select="node()"/>
        </xsl:copy>
      </xsl:when>
      <xsl:when test="exists($captor)">
        <xsl:copy>
          <xsl:attribute name="base" select="concat(if (exists(@method)) then '.' else '', $captor/@name)"/>
          <xsl:apply-templates select="@* except @base"/>
          <xsl:apply-templates select="node()"/>
        </xsl:copy>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="@*"/>
          <xsl:apply-templates select="node()"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="/object">
    <xsl:copy>
      <xsl:apply-templates select="(node() except errors)|@*"/>
      <xsl:variable name="errors" as="element()*">
        <xsl:for-each-group select="//o[@local]" group-by="concat(@local, '#', generate-id(ancestor::o[not(@base)][1]))">
          <xsl:if test="count(current-group()) &gt; 1">
            <error>
              <xsl:attribute name="check" select="'resolve-local-names'"/>
              <xsl:attribute name="line" select="if (current-group()[2]/@line) then current-group()[2]/@line else 0"/>
              <xsl:attribute name="severity" select="'error'"/>
              <xsl:text>duplicate local name '</xsl:text>
              <xsl:value-of select="current-group()[1]/@local"/>
              <xsl:text>'</xsl:text>
            </error>
          </xsl:if>
        </xsl:for-each-group>
      </xsl:variable>
      <xsl:if test="not(empty($errors)) or exists(/object/errors)">
        <errors>
          <xsl:apply-templates select="/object/errors/error"/>
          <xsl:copy-of select="$errors"/>
        </errors>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
