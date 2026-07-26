<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:eo="https://www.eolang.org" exclude-result-prefixes="xs eo" id="resolve-local-names" version="2.0">
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
  search: "$.foo" reaches the innermost formation and "^.foo" its parent
  (§3.14), and each binds to the handle of exactly the formation it names
  (see `eo:scope`, #5917). Such a reference is the only spelling left when
  the nested formation rebinds the name it reads - `^.foo &gt; foo` copies
  the parent's handle into an attribute of the same name, where a bare "foo"
  would bind to the copy - so leaving it unresolved stranded the handle
  behind a name nothing owned, and the reference dataized to ⊥.
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
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
  descendant has that formation as its nearest enclosing formation - the "is"
  test stops the search at a nested formation boundary, so a handle never
  leaks up out of, or sideways between, nested formations (#5780).
  -->
  <xsl:function name="eo:captor" as="element()?">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="name" as="xs:string" select="if (exists($ref/@method)) then substring-after($ref/@base, '.') else string($ref/@base)"/>
    <xsl:variable name="scope" as="element()?" select="if (exists($ref/@method)) then eo:scope($ref) else ($ref/ancestor::o[not(@base)][o[@name=$name] or (some $d in descendant::o[@local=$name] satisfies $d/ancestor::o[not(@base)][1] is .)])[last()]"/>
    <xsl:sequence select="$scope/descendant::o[@local=$name][ancestor::o[not(@base)][1] is $scope][1]"/>
  </xsl:function>
  <!--
  The formation that the explicit receiver of a dispatch names: "ξ" (written
  "$") is the innermost formation, "ρ" ("^") is its parent, and every further
  ".ρ" segment climbs one level more. Such a receiver pins the scope instead
  of leaving it to the search above, so "^.foo" reaches exactly one formation
  out and stops there, however many scopes further out also declare the name.
  A dispatch is still flat at this point in the train - "wrap-method-calls"
  nests it later - so the receiver of a "@method" node is its preceding
  sibling; a reversed dispatch ("plus. &gt; y") carries no "@method" and holds
  its receiver as a child, so it never reaches here. The empty sequence for
  every other receiver (an application, a global, a name), which names no
  scope known at parse time.
  -->
  <xsl:function name="eo:scope" as="element()?">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="receiver" as="element()?" select="$ref/preceding-sibling::o[1]"/>
    <xsl:sequence select="if (empty($receiver/@base)) then () else if ($receiver/@base='ξ' and empty($receiver/@method)) then $ref/ancestor::o[not(@base)][1] else if ($receiver/@base='ρ' and empty($receiver/@method)) then $ref/ancestor::o[not(@base)][2] else if ($receiver/@base='.ρ' and exists($receiver/@method)) then eo:scope($receiver)/ancestor::o[not(@base)][1] else ()"/>
  </xsl:function>
  <xsl:template match="o[@base and exists(eo:captor(.))]">
    <xsl:copy>
      <xsl:attribute name="base" select="concat(if (exists(@method)) then '.' else '', eo:captor(.)/@name)"/>
      <xsl:apply-templates select="@* except @base"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="/object">
    <xsl:copy>
      <xsl:apply-templates select="(node() except errors)|@*"/>
      <xsl:variable name="errors" as="element()*">
        <xsl:for-each-group select="//o[@local]" group-by="concat(@local, '#', generate-id(ancestor::o[not(@base)][1]))">
          <xsl:if test="count(current-group()) &gt; 1">
            <xsl:element name="error">
              <xsl:attribute name="check" select="'resolve-local-names'"/>
              <xsl:attribute name="line" select="if (current-group()[2]/@line) then current-group()[2]/@line else 0"/>
              <xsl:attribute name="severity" select="'error'"/>
              <xsl:text>duplicate local name '</xsl:text>
              <xsl:value-of select="current-group()[1]/@local"/>
              <xsl:text>'</xsl:text>
            </xsl:element>
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
