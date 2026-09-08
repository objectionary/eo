<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs eo" id="merge-monikers" version="2.0">
  <!--
  Merges a standalone named binding back onto its bare-reference use site,
  restoring the shorter "moniker" spelling. The parser hoists a `> name`
  binding up to its enclosing formation regardless of where it is written,
  so the moniker spelling (the binding written in place of its bare
  reference, such as `number num.as-bytes > value` sitting where a bare
  `value` would appear) and the expanded spelling the printer would
  otherwise emit (a bare `value` reference plus a separate `... > value`
  binding line) parse to one and the same object graph. This pass turns
  the expanded spelling back into the moniker.

  For each formation, an eligible bound attribute (see `eo:moniker-binding`)
  is merged onto the first hostable reference reachable through no intervening
  formation (so the name still binds to the same formation). A bare `ξ.<name>`
  reference must be unnamed, but a dispatch chain `ξ.<name>.<seg1>.<seg2>…`
  (#5782, #5970) may also host onto a named reference (such as `q. > @`), since
  the reversed dispatch keeps the reference's own `@name` (#5794); a bare
  reference carrying arguments cannot host the binding and stays a reference.
  When no
  hostable reference exists, the binding is left in place.

  A binding with several hostable references still becomes a moniker,
  landing on the first one to print (#5739, #7218), which need not be the
  first in document order, since canonical attribute ordering (#5706)
  reorders the sibling bindings a reference sits inside.
  -->
  <xsl:import href="/org/eolang/parser/_funcs.xsl"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!--
  The `ξ.` prefix every local reference carries, built once instead of at each
  of the many `starts-with`/`substring-after` calls below. Saxon does not fold
  `concat($eo:xi, '.')` into a constant, since `$eo:xi` is a global variable
  rather than a literal, so leaving it inline rebuilds the same two-character
  string for every reference the sheet looks at.
  -->
  <xsl:variable name="eo:xi-dot" select="concat($eo:xi, '.')"/>
  <!-- The cactus-name prefix, hoisted for the same reason as "eo:xi-dot". -->
  <xsl:variable name="eo:cactus-name" select="concat('a', $eo:cactoos)"/>
  <!--
  Every reference that resolves to an attribute name (`eo:resolved-ref`),
  indexed by the formation that owns it and the name it resolves to. The
  lookups below all ask the same question — "which references in this
  formation name this binding?" — which as a `$owner//o[...]` scan costs a walk
  of the whole subtree per binding, and so a walk of the document per node
  once every node asks it. The index answers it in constant time, and, being a
  key, still hands the references back in document order, which is what makes
  "the first hosting reference" mean the same thing as before.
  -->
  <xsl:key name="moniker-ref" match="o[eo:resolved-ref(.) != '']" use="concat(generate-id(ancestor::o[eo:abstract(.)][1]), ' ', eo:resolved-ref(.))"/>
  <!--
  Every applied reference (`eo:applied-refs`) — a bare `ξ.<name>` carrying
  arguments and no name of its own — indexed by owning formation and receiver
  name, the applied twin of `moniker-ref` above.
  -->
  <xsl:key name="applied-ref" match="o[exists(@base) and starts-with(@base, $eo:xi-dot) and exists(o) and not(exists(@name))]" use="concat(generate-id(ancestor::o[eo:abstract(.)][1]), ' ', substring-after(@base, $eo:xi-dot))"/>
  <!--
  Every eligible binding (`eo:moniker-binding`) indexed by its owning formation
  and name, so a reference reaches the binding it resolves to without scanning
  the formation's attributes.
  -->
  <xsl:key name="moniker-binding" match="o[eo:moniker-binding(.)]" use="concat(generate-id(..), ' ', @name)"/>
  <!--
  The same bindings indexed by name alone, for the two "kept reference"
  lookups, which climb the ancestor formations looking for whichever one
  declares a segment of the reference's base. Cactus names are auto-generated
  and effectively unique, so this bucket holds one node and the climb becomes a
  parentage check on it rather than a scan of every ancestor's attributes.
  A handle floated into a pipe is indexed here too, even though it is not a
  merge candidate itself: "inline-cactoos" mints a pipe for a handle whose
  value applies a formation it relocates (#7863), and the readers of such a
  handle are left on the cactus name, which only these lookups put back.
  -->
  <xsl:key name="moniker-name" match="o[eo:moniker-binding(.) or (exists(@pipe) and exists(@local))]" use="@name"/>
  <!--
  The single attribute name a hostable `ξ.<name>` reference resolves to, or
  the empty string for anything that is not hostable. Two shapes host a
  binding: a bare reference `ξ.<name>` (no trailing path, no arguments) and a
  trailing dispatch chain `ξ.<name>.<seg1>.<seg2>…`, a reversed dispatch
  spelled forward whose receiver `<name>` can be inlined exactly as the bare
  case is, only as the innermost reversed dispatch's receiver rather than a
  plain one (#5782, #5970). Both resolve to `<name>`. A non-`ξ` base
  disqualifies it, as does a named node for the bare case only — a bare inline
  would clobber the reference's `@name`, but a dispatch keeps it (#5794).
  -->
  <xsl:function name="eo:resolved-ref" as="xs:string">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="tail" select="substring-after($ref/@base, $eo:xi-dot)"/>
    <xsl:sequence select="if (not(starts-with($ref/@base, $eo:xi-dot))) then '' else if (not(exists($ref/@name)) and not(contains($tail, '.')) and not($ref/o)) then $tail else if (eo:dispatch-seg($ref) != '') then substring-before($tail, '.') else ''"/>
  </xsl:function>
  <!--
  The trailing dispatch chain of a hostable dispatch reference
  `ξ.<name>.<seg1>.<seg2>…` — every segment after the receiver name `<name>`,
  joined by dots — or the empty string for a bare reference or a non-`ξ` base.
  A single trailing segment (#5782) folds to one reversed dispatch over the
  inlined receiver; a multi-segment chain (#5970) folds to one reversed dispatch
  per segment nested inside each other, since `<name>.seg1.seg2` and
  `seg2. (seg1. <name>)` denote the same graph. Unlike a bare reference, a
  dispatch may carry arguments and may be a named node (#5794).

  The "ξ." test comes first, ahead of the tests it used to sit behind: this
  function and "eo:resolved-ref" above run for every object in the file, and
  most bases are not rooted at "ξ." at all (#7938).
  -->
  <xsl:function name="eo:dispatch-seg" as="xs:string">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="tail" select="substring-after($ref/@base, $eo:xi-dot)"/>
    <xsl:sequence select="if (not(starts-with($ref/@base, $eo:xi-dot))) then '' else if (contains($tail, '.') and substring-before($tail, '.') != '') then substring-after($tail, '.') else ''"/>
  </xsl:function>
  <!--
  Whether `$attr` is a restored recursive `&gt;&gt; name` handle: an abstract
  formation carrying its readable "@local" handle whose visible "@name" has
  already been promoted to that same handle by "restore-local-names" (which
  sets "@name = @local" and rewrites the self-references only for a recursive
  formation, R-3.10.12 / #5681). Its cactus name is gone, yet when it is used
  as a method-dispatch receiver it must still host onto that dispatch as a
  reversed-dispatch moniker (#5848) — the recursive mirror of the non-recursive
  dispatch fold, where the anonymous formation is inlined (#5782). A void
  ("@base=∅"), a const handle (cactus "@name" ≠ readable "@local") and a plain
  cactus binding all fail "@name = @local", so only the restored recursive
  handle is admitted here.
  -->
  <xsl:function name="eo:recursive-handle" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:sequence select="eo:abstract($attr) and exists($attr/@local) and exists($attr/@name) and $attr/@name = $attr/@local"/>
  </xsl:function>
  <!--
  Whether `$attr` is a formation attribute eligible to become a moniker:
  auto-named with the cactus prefix (`a🌵`, §9.2), either bound (has a base)
  or an abstract formation (#5794), and neither void, `φ`, a test, nor already
  floated with a pipe (`|`). Only the compiler's obfuscated names are merged;
  a real, author-chosen name such as `value` reads best on its own
  `... > name` line and stays standalone (#5738). A restored recursive handle
  (see `eo:recursive-handle`) is the exception: its readable name is compiler
  plumbing that the non-recursive path discards entirely, so it too may host
  onto a dispatch use (#5848). Its bare uses always carry a `@name` (the
  `| ... > name` pipe #5837/#5848 leaves them with) and so never resolve as a
  hostable bare reference, so only a genuine dispatch use folds.
  -->
  <xsl:function name="eo:moniker-binding" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:sequence select="exists($attr/@name) and (starts-with($attr/@name, $eo:cactus-name) or eo:recursive-handle($attr)) and (exists($attr/@base) or eo:abstract($attr)) and not(exists($attr/@pipe)) and not(eo:void($attr)) and $attr/@name != $eo:phi and not(eo:test-attr($attr)) and eo:abstract($attr/..)"/>
  </xsl:function>
  <!--
  The key `to-eo-tree.xsl` sorts a reference's own top-level binding under:
  an alphabetical run by `(@local, @name)[1]`, with a non-sortable, void, `φ`
  or test binding in its own bucket. Hosting by this key, not by document
  order, makes a reprint settle instead of moving the binding again.
  -->
  <xsl:function name="eo:host-key" as="xs:string">
    <xsl:param name="ref" as="element()"/>
    <xsl:param name="owner" as="element()"/>
    <xsl:variable name="binding" select="$ref/ancestor-or-self::o[parent::*[. is $owner]][1]"/>
    <xsl:variable name="bucket" select="if (not(eo:abstract($owner) and empty($owner/o[@pipe]))) then 0 else if (eo:void($binding)) then 1 else if ($binding/@name = $eo:phi) then 2 else if (eo:test-attr($binding)) then 4 else 3"/>
    <xsl:sequence select="concat($bucket, ' ', if ($bucket = (0, 1, 2)) then '' else string(($binding/@local, $binding/@name)[1]))"/>
  </xsl:function>
  <!--
  The references that can host the binding `$attr`, shortest spelling first: a
  bare `ξ.<name>` reference or a dispatch chain `ξ.<name>.<seg1>.<seg2>…`
  (#5782, #5970) whose nearest formation ancestor is the binding's own owner and
  that does not sit inside the binding itself. Bare references are listed first,
  so a binding with both spellings still folds into the shorter bare inline and
  a dispatch hosts only when no bare reference is available. The dispatches
  themselves are ordered by how many segments they carry, fewest first (a
  single `<seg>` before a `<seg1>.<seg2>` chain), and by document order within
  an equal count, so the shortest, least-nested moniker wins and a multi-segment
  chain hosts only when no shorter reference can (#5970) — the previously
  stranded tail #5782 left expanded. A named handle (see `eo:named-handle`)
  reorders the two spellings: a dispatch host is listed first and a bare host
  last, the reverse of the default. A bare host once dropped the readable name
  its other references read back through (#5810), so it was skipped entirely and
  a handle reached by bare references alone stayed standing (#5956); the
  merge template now keeps that name at a bare site too (see `eo:named-handle`
  in the keep-list), so the bare reference hosts when no dispatch does (#5996),
  and the readable `&gt;&gt; name` still travels to whichever use site wins.
  -->
  <xsl:function name="eo:moniker-refs" as="element()*">
    <xsl:param name="attr" as="element()"/>
    <xsl:variable name="owner" select="$attr/.."/>
    <xsl:variable name="unordered" select="key('moniker-ref', concat(generate-id($owner), ' ', $attr/@name), root($attr))[not(ancestor::o[. is $attr])]"/>
    <xsl:variable name="refs" as="element()*">
      <xsl:perform-sort select="$unordered">
        <xsl:sort select="eo:host-key(., $owner)"/>
      </xsl:perform-sort>
    </xsl:variable>
    <xsl:variable name="dispatches" select="$refs[eo:dispatch-seg(.) != '']"/>
    <xsl:variable name="dispatch" as="element()*">
      <xsl:choose>
        <xsl:when test="exists($dispatches[2])">
          <xsl:perform-sort select="$dispatches">
            <xsl:sort select="count(tokenize(eo:dispatch-seg(.), '\.'))" data-type="number" order="ascending"/>
          </xsl:perform-sort>
        </xsl:when>
        <xsl:otherwise>
          <xsl:sequence select="$dispatches"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:sequence select="if (eo:named-handle($attr)) then ($dispatch, $refs[eo:dispatch-seg(.) = '']) else ($refs[eo:dispatch-seg(.) = ''], $dispatch)"/>
  </xsl:function>
  <!--
  Whether `$attr` is a based `>> name` handle whose readable name must outlive
  the merge: a plain reference or application (not an abstract formation)
  carrying its "@local" handle, which "restore-local-names" leaves there only
  when the binding survives "inline-cactoos" with references still reading it
  by name — shared (#5944, #5956) or unreached (#5914). A const handle is
  excluded: it keeps its "@name" and "@local" through a bare inline too (#5828),
  so it needs no such restriction. An abstract formation carries its name
  wherever it lands and is excluded for the same reason.
  -->
  <xsl:function name="eo:named-handle" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:sequence select="exists($attr/@local) and not(eo:abstract($attr)) and not(eo:const-handle($attr))"/>
  </xsl:function>
  <!--
  The binding that a reference `$ref` should be replaced with, or the empty
  sequence when `$ref` hosts no binding (not a bare reference, no eligible
  binding, or not the first hosting reference).
  -->
  <xsl:function name="eo:hosted-binding" as="element()*">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="name" select="eo:resolved-ref($ref)"/>
    <xsl:choose>
      <!--
      Nothing but a "ξ.&lt;name&gt;" reference can host a binding, and most
      objects are not one. Answering that first keeps the ancestor climb, the
      "generate-id" and the key probe off the path every object walks: this
      runs from a match pattern (#7938).
      -->
      <xsl:when test="$name = ''"/>
      <xsl:otherwise>
        <xsl:variable name="owner" select="$ref/ancestor::o[eo:abstract(.)][1]"/>
        <xsl:variable name="binding" select="key('moniker-binding', concat(generate-id($owner), ' ', $name), root($ref))[1]"/>
        <xsl:sequence select="if (exists($binding) and (eo:moniker-refs($binding)[1] is $ref)) then $binding else ()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  Whether the formation `$attr` prints as a multi-line block rather than the
  compact one-line "&lt;value&gt; &gt; [params] &gt;&gt; name" suffix form (Pretty's
  inline-phi), which needs a single "@" decoratee whose value is flat. So a
  formation is a block when it carries more than one body binding (like "digits",
  with both "@" and "q"), its sole binding is not that "@" decoratee, or that
  decoratee is compound — an application whose arguments carry children of their
  own (like "negative", whose "@" is an "if." over three nested branches). Only a
  block folds an applied reference in an argument slot: it forces the enclosing
  list vertical, so the "| args" pipe gets its own line rather than the
  unparsable "|:0" (#5983). A flat single-"@" formation ("a.plus b > @") stays
  inline and keeps its applied reference standing (#5983), the "bar 1 2" case.
  -->
  <xsl:function name="eo:block-handle" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:variable name="body" select="$attr/o[not(eo:void(.))]"/>
    <xsl:variable name="phi" select="$body[@name = $eo:phi]"/>
    <xsl:sequence select="eo:abstract($attr) and (count($body) &gt; 1 or empty($phi) or exists($phi/o/o))"/>
  </xsl:function>
  <!--
  The references that host the binding `$attr` as an applied moniker. An applied
  reference is a bare "ξ.&lt;name&gt;" formation handle carrying argument children
  — "handle args", a dispatch receiver ("(handle args).read") or a list element
  ("directory.eo"'s "r (walk ...)" in "seq *"). Since neither a bare inline (drops
  the arguments, #5834) nor a reversed dispatch can host it, the handle is inlined
  in place and its arguments become a "| args" pipe continuation (#5848), the
  recursive mirror of "inline-cactoos" (#5844). Hosts onto the first such reference
  only, excluding the binding's own subtree. A reference with its own "@name" is
  excluded — a "| args &gt; name" pipe already folded by "restore-local-names"
  (#5837/#5848); a nameless one round-trips. Recursion is not what admits the
  fold — a restored recursive handle (`eo:recursive-handle`) is just one kind of
  formation handle reached this way; a plain "&gt;&gt; name" formation reached only
  by applications folds identically (#6008). What gates the fold instead is
  `eo:applied-hosted`.
  -->
  <xsl:function name="eo:applied-refs" as="element()*">
    <xsl:param name="attr" as="element()*"/>
    <xsl:variable name="owner" select="$attr/.."/>
    <xsl:sequence select="if (empty($attr)) then () else key('applied-ref', concat(generate-id($owner), ' ', $attr/@name), root($attr))[not(ancestor::o[. is $attr])]"/>
  </xsl:function>
  <!--
  Whether the applied reference `$ref` sits in a dispatch-receiver slot: the
  first child (the "ρ" receiver) of a reversed dispatch ("@base" starting with a
  dot), as in "(handle args).read" (#5844/#5848). "unnecessary-as" strips the
  pipe's "@as" cleanly for a reversed dispatch, so a receiver never risks the
  "|:0" an argument-slot fold does (#5983) and folds whatever shape the formation
  prints as; any other position folds only when the formation is a block.
  -->
  <xsl:function name="eo:receiver-ref" as="xs:boolean">
    <xsl:param name="ref" as="element()"/>
    <xsl:sequence select="exists($ref/parent::o[starts-with(@base, '.')]) and empty($ref/preceding-sibling::o)"/>
  </xsl:function>
  <!--
  Whether the formation handle `$attr` folds onto an applied reference at all: an
  abstract formation (only a formation inlines as a "&gt;&gt; name" moniker over its
  "| args" pipe; a based handle reapplied further has no such spelling and stays
  standing, #5952) reached by such a reference (`eo:applied-refs`), whose first
  reference is a dispatch receiver (`eo:receiver-ref`, always safe) or which
  prints as a block (`eo:block-handle`, so an argument-slot pipe avoids "|:0",
  #5983). Recursion plays no part: #5848 folded recursive handles only because
  those were the sole ones "inline-cactoos" left standing to reach here; a plain
  formation handle now reaches here too (kept standing by #5983's
  `eo:arg-applied`) and folds by the very same rule (#6008).
  -->
  <xsl:function name="eo:applied-hosted" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:variable name="refs" select="eo:applied-refs($attr)"/>
    <xsl:sequence select="exists($refs) and eo:abstract($attr) and (eo:receiver-ref($refs[1]) or eo:block-handle($attr))"/>
  </xsl:function>
  <xsl:function name="eo:applied-handle" as="element()*">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="name" select="substring-after($ref/@base, $eo:xi-dot)"/>
    <xsl:choose>
      <!-- Shape tests first, for the reason on "eo:hosted-binding" above. -->
      <xsl:when test="not($name != '' and not(contains($name, '.')) and exists($ref/o) and starts-with($ref/@base, $eo:xi-dot))"/>
      <xsl:otherwise>
        <xsl:variable name="owner" select="$ref/ancestor::o[eo:abstract(.)][1]"/>
        <xsl:variable name="binding" select="key('moniker-binding', concat(generate-id($owner), ' ', $name), root($ref))[eo:applied-hosted(.)][1]"/>
        <xsl:sequence select="if (exists($binding) and (eo:applied-refs($binding)[1] is $ref)) then $binding else ()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  Whether `$attr` is a const file-local handle (`a &gt;&gt; b!`, R-3.10.12):
  a based binding (not an abstract formation) carrying both the `!` const
  marker and its readable "@local" handle. Such a handle is dataized once and
  cached, so a multi-referenced one is kept whole by "inline-cactoos" (#5828)
  and reaches here as an ordinary moniker binding. Unlike the non-const based
  handle (#5810), it keeps its obfuscated "@name" and "@local" when hosted, so
  "to-eo-tree" prints the merged binding as `a &gt;&gt; b!` rather than the
  anonymous `a!`.
  -->
  <xsl:function name="eo:const-handle" as="xs:boolean">
    <xsl:param name="attr" as="element()"/>
    <xsl:sequence select="exists($attr/@const) and exists($attr/@local) and not(eo:abstract($attr))"/>
  </xsl:function>
  <!--
  The const file-local handle binding a reference `$ref` resolves to but does
  NOT host (the binding folds onto its first reference only). Every other
  reference keeps the readable "@local" handle in place of the obfuscated
  cactus name, so it reads back as a bare `b` (or a dispatch `b.seg`) rather
  than a synthetic "vL_P" name (#5828). The reference is matched by carrying the
  binding's cactus "@name" as one of its base segments — a bare `ξ.<name>`, a
  single-segment dispatch `ξ.<name>.<seg>`, or a multi-segment chain
  `ξ.<name>.<seg>.<seg>` whose receiver position the moniker fold cannot host
  (#5890) — so a stranded receiver reference reads back by name instead of a
  synthetic id, the const mirror of `eo:kept-local-ref`. The reference need not
  live in the binding's own scope: one written inside a nested formation climbs
  to the handle through `ρ` and is stranded just the same (#5893), so the binding
  is looked up in the nearest enclosing formation that declares it. The binding's
  own subtree and the single hosting reference are excluded.
  -->
  <xsl:function name="eo:kept-const-ref" as="element()*">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="candidates" select="key('moniker-name', tokenize($ref/@base, '\.'), root($ref))[eo:const-handle(.)][some $scope in $ref/ancestor::o satisfies $scope is ..]"/>
    <xsl:variable name="binding" select="$candidates[last()]"/>
    <xsl:sequence select="if (exists($binding) and not($ref is $binding) and not($ref/ancestor::o[. is $binding]) and (exists($binding/@pipe) or not(eo:moniker-refs($binding)[1] is $ref))) then $binding else ()"/>
  </xsl:function>
  <!--
  Whether the bare name of `$binding` would read as something else at `$ref`:
  some formation between the reference and the one that owns the handle declares
  that same readable name, so the parser's search — which stops at the nearest
  scope declaring the name at all — never reaches the handle. The binding a
  reference stands in counts as such a declaration, and is the reason this
  happens at all: `^.foo &gt; foo` copies the parent's handle into an attribute
  of the same name, and inside that formation a bare `foo` is the copy (#5917).
  -->
  <xsl:function name="eo:shadowed" as="xs:boolean">
    <xsl:param name="ref" as="element()"/>
    <xsl:param name="binding" as="element()"/>
    <xsl:variable name="owner" select="$binding/.."/>
    <xsl:sequence select="some $scope in $ref/ancestor::o[eo:abstract(.)][ancestor::o[. is $owner]] satisfies exists($scope/o[@name = $binding/@local or @local = $binding/@local])"/>
  </xsl:function>
  <!--
  The base under which a non-hosting reference reads back: the readable "@local"
  handle of the binding it resolves to, plus whatever the reference dispatches on
  top of it. The obfuscated cactus segment gives way to that handle, and the
  `ξ.ρ...` climb in front of it to a plain `ξ`, since a handle is reached by its
  bare name alone and the parser re-derives the climb in "resolve-local-names".
  A reference standing in the binding's own scope has no climb to shed; one
  inside a nested formation does, and #5893 spelled it out as a `^.&lt;local&gt;`
  that bound to nothing on reparse. It binds now (#5917), and a shadowed
  reference (see `eo:shadowed`) keeps its climb for that reason: the bare name
  is taken there, so the explicit scope is the only spelling that still reads
  back as the handle.
  -->
  <xsl:function name="eo:handle-base" as="xs:string">
    <xsl:param name="ref" as="element()"/>
    <xsl:param name="binding" as="element()"/>
    <xsl:variable name="segs" select="tokenize($ref/@base, '\.')"/>
    <xsl:variable name="at" select="index-of($segs, string($binding/@name))[1]"/>
    <xsl:sequence select="string-join((if (eo:shadowed($ref, $binding)) then subsequence($segs, 1, $at - 1) else $eo:xi, string($binding/@local), subsequence($segs, $at + 1)), '.')"/>
  </xsl:function>
  <!--
  The kept multi-referenced handle binding a reference `$ref` resolves to but does
  NOT host (the binding folds onto its first reference only,
  `eo:moniker-refs`). Any other reference — a named alias
  `name &gt; a` or a further `name.seg` dispatch that the moniker fold leaves
  in place — keeps the readable "@local" handle in place of the obfuscated
  cactus name, so it reads back as `name` (or `name.seg`) rather than a
  synthetic "vL_P" placeholder, the non-const mirror of `eo:kept-const-ref`
  (which covers the const handles this one leaves out). Both shapes of handle
  reach here: an abstract formation kept whole because it is shared (#5876),
  and a based handle (`a.b &gt;&gt; name`) kept because it is shared too — by a
  method dispatch, which "inline-cactoos" never inlines (#5944), or by a bare
  reference, which it no longer folds a second copy into (#5956). The
  reference is matched by carrying the binding's cactus "@name" as one of its
  base segments; the binding's own subtree is excluded. As with the const
  handle, the reference need not live in the binding's own scope: two mutually
  recursive helpers call each other from sibling formations, so a reference
  climbs out of its own scope to reach the handle and is stranded just the same
  (#5907), and the binding is looked up in the nearest enclosing formation that
  declares it.
  -->
  <xsl:function name="eo:kept-local-ref" as="element()*">
    <xsl:param name="ref" as="element()"/>
    <xsl:variable name="candidates" select="key('moniker-name', tokenize($ref/@base, '\.'), root($ref))[not(eo:const-handle(.)) and exists(@local)][some $scope in $ref/ancestor::o satisfies $scope is ..]"/>
    <xsl:variable name="binding" select="$candidates[last()]"/>
    <xsl:sequence select="if (exists($binding) and not($ref is $binding) and not($ref/ancestor::o[. is $binding]) and (exists($binding/@pipe) or not(eo:moniker-refs($binding)[1] is $ref))) then $binding else ()"/>
  </xsl:function>
  <!--
  The binding, out of those candidates, that a reference actually keeps: not
  the binding itself, not a reference inside it, and not the one reference the
  moniker fold hosts the binding onto (a binding floated into a pipe has no
  hosting reference to spare, so every reader of it keeps the name).
  -->
  <!--
  The handle a non-hosting reference reads, whichever kind it is - the two
  readings above over one shared candidate list. They used to be a template
  each, of equal priority and each computing that list for itself, so every
  "@base" in the document paid for the lookup twice; a reference both would
  have fired on still takes the non-const reading, which is the one Saxon
  picked when the two templates collided, the later of two equal-priority
  rules winning (#7938).
  -->
  <!--
  Replace the first hosting reference with the merged binding, always keeping
  the reference's positional `@as`. A bare reference becomes the binding
  inlined in place: the binding's other attributes and its children. An
  abstract formation keeps its obfuscated `@name`, which `to-eo-tree` renders
  as the anonymous `[...] >>` marker; a based binding — a `>> name` handle
  whose value is a plain reference such as `a >> b` (R-3.10.12) — drops its
  `@name` (and the `@local` handle it leaves behind) at a single-reference site,
  since the bare reference is unnamed and carrying the obfuscated name over would
  turn the inline into a spurious named node that `to-eo-tree` prints as its own
  `a >>` line instead of an anonymous argument (#5810). A const based handle
  (`a >> b!`, see `eo:const-handle`) keeps its `@name` and
  `@local`, so `to-eo-tree` prints the readable `a >> b!` and its other
  references read back as the bare handle `b` (#5828); a named handle
  (see `eo:named-handle`) reached by bare references alone does the same, keeping
  its `@name` and `@local` so the readable `a >> b` travels to the bare use it
  hosts onto while the remaining bare references still read it back (#5996). A dispatch chain
  `ξ.<name>.<seg1>.<seg2>…` becomes a reversed dispatch, one level per segment
  (see `eo:wrap-dispatch`): the innermost hosts the inlined binding, the
  outermost carries the reference's own children as arguments (#5782, #5970).
  The dispatch also keeps the reference's own `@name`, `@local` and `@const`, so
  a named use such as `q. > @` over an anonymous formation round-trips (#5794),
  a const one such as `c.gte 2 > a!` keeps its `!` (#5900) — that marker is what
  makes the object dataized once and cached, so losing it would silently turn a
  cached object into one recomputed at every use — and a handle hosting another
  handle in its own receiver (`data.size >> len!` over `b >> data!`) keeps its
  readable name instead of printing as an anonymous `size. >>!` that its own
  references no longer resolve to (#5914).

  The binding lands at the reference through the "merged" mode below, which
  carries whatever the binding hosts down with it (#5918).
  -->
  <xsl:template match="o[starts-with(@base, $eo:xi-dot)][exists(eo:hosted-binding(.))]" priority="1">
    <xsl:variable name="owner" select="ancestor::o[eo:abstract(.)][1]"/>
    <xsl:variable name="binding" select="key('moniker-binding', concat(generate-id($owner), ' ', eo:resolved-ref(.)), root(.))[1]"/>
    <xsl:variable name="seg" select="eo:dispatch-seg(.)"/>
    <xsl:choose>
      <xsl:when test="$seg = ''">
        <xsl:variable name="merged" as="element()">
          <xsl:apply-templates select="$binding" mode="merged"/>
        </xsl:variable>
        <o>
          <xsl:apply-templates select="@as"/>
          <xsl:copy-of select="$merged/@*[eo:abstract($merged) or eo:const-handle($merged) or eo:named-handle($merged) or (name() != 'name' and name() != 'local')]"/>
          <xsl:copy-of select="$merged/node()"/>
        </o>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="receiver" as="node()*">
          <xsl:apply-templates select="$binding" mode="merged"/>
        </xsl:variable>
        <xsl:variable name="args" as="node()*">
          <xsl:apply-templates select="o"/>
        </xsl:variable>
        <xsl:call-template name="eo:wrap-dispatch">
          <xsl:with-param name="segs" select="tokenize($seg, '\.')"/>
          <xsl:with-param name="receiver" select="$receiver"/>
          <xsl:with-param name="args" select="$args"/>
          <xsl:with-param name="attrs" select="@as|@name|@local|@const"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!--
  Build the reversed-dispatch spelling of a hosted dispatch reference
  `ξ.<name>.<seg1>.<seg2>…` as nested `o` levels: the outermost `<segN>.` carries
  the reference's own `$args` and `$attrs` (`@as`, `@name`, `@local`, `@const`),
  each further level wraps the one below, and the innermost `<seg1>.` holds the
  inlined `$receiver`. A single segment (#5782) degenerates to one level; a
  multi-segment chain (#5970) nests one reversed dispatch per segment.
  -->
  <xsl:template name="eo:wrap-dispatch">
    <xsl:param name="segs" as="xs:string*"/>
    <xsl:param name="receiver" as="node()*"/>
    <xsl:param name="args" as="node()*" select="()"/>
    <xsl:param name="attrs" as="attribute()*" select="()"/>
    <o>
      <xsl:copy-of select="$attrs"/>
      <xsl:attribute name="base" select="concat('.', $segs[last()])"/>
      <xsl:choose>
        <xsl:when test="count($segs) gt 1">
          <xsl:call-template name="eo:wrap-dispatch">
            <xsl:with-param name="segs" select="$segs[position() lt last()]"/>
            <xsl:with-param name="receiver" select="$receiver"/>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="$receiver"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:copy-of select="$args"/>
    </o>
  </xsl:template>
  <!--
  The binding as it lands at the reference it was merged onto: itself, with its
  own references merged in turn and its positional `@as` left behind (the
  reference above supplies the slot). A binding is often a host site itself: the
  parser hoists a handle written inside another handle's receiver out to its own
  binding (`text &gt;&gt; bts!` under `size. &gt;&gt; len!`) and leaves only a
  reference behind, so `len`'s own `@base` is what `bts` folds onto. Rebuilding
  such a binding attribute by attribute would leave that fold undone at the new
  site while `bts` is dropped as merged, printing a receiver that binds to
  nothing (#5918); instead the reversed dispatch is built here exactly as above,
  and recursively, so a whole chain of handles travels to the use site nested
  inside one another. The chain travelled so far comes along in `$seen`, since
  two handles can dispatch on each other (`p.plus 1 >> a!` beside
  `a.plus 2 >> p!`) — source that never dataizes, but that the parser accepts —
  and a repeat has to end the descent rather than recur forever.
  -->
  <xsl:template match="o" mode="merged">
    <xsl:param name="seen" as="element()*" select="()"/>
    <xsl:variable name="binding" select="eo:hosted-binding(.)[every $node in $seen satisfies not(. is $node)]"/>
    <xsl:choose>
      <xsl:when test="exists($binding)">
        <xsl:variable name="receiver" as="node()*">
          <xsl:apply-templates select="$binding" mode="merged">
            <xsl:with-param name="seen" select="($seen, .)"/>
          </xsl:apply-templates>
        </xsl:variable>
        <xsl:variable name="args" as="node()*">
          <xsl:apply-templates select="node()"/>
        </xsl:variable>
        <xsl:call-template name="eo:wrap-dispatch">
          <xsl:with-param name="segs" select="tokenize(eo:dispatch-seg(.), '\.')"/>
          <xsl:with-param name="receiver" select="$receiver"/>
          <xsl:with-param name="args" select="$args"/>
          <xsl:with-param name="attrs" select="@*[name() != 'as' and name() != 'base']"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <o>
          <xsl:apply-templates select="@*[name() != 'as']|node()"/>
        </o>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!--
  Host an applied formation handle (see `eo:applied-handle`): emit the inlined
  handle formation in place of the reference, then a "@pipe" node carrying the
  reference's arguments and pointing its base back at the formation's name.
  "to-eo-tree" renders the formation as the "&gt;&gt; name" receiver and the
  pipe node as "| args" because its base equals the preceding sibling's name —
  so an applied formation handle folds to the handle moniker plus a pipe
  continuation carrying the arguments (#5848 for the recursive twin, #6008 for
  the plain one), the mirror of #5844. The reference's own positional "@as" is
  kept on the pipe so an argument slot survives; the standalone binding is
  dropped below.
  -->
  <xsl:template match="o[starts-with(@base, $eo:xi-dot)][exists(o)][not(exists(@name))][exists(eo:applied-handle(.))]" priority="2">
    <xsl:variable name="binding" select="eo:applied-handle(.)"/>
    <xsl:for-each select="$binding">
      <xsl:copy>
        <xsl:apply-templates select="node()|@*"/>
      </xsl:copy>
    </xsl:for-each>
    <o>
      <xsl:apply-templates select="@as"/>
      <xsl:attribute name="pipe"/>
      <xsl:attribute name="base" select="@base"/>
      <xsl:apply-templates select="o"/>
    </o>
  </xsl:template>
  <!--
  Rewrite a non-hosting reference to a kept handle from the obfuscated cactus
  name back to the readable "@local" handle, so it reads as `name` (or
  `name.seg`) instead of a synthetic "vL_P" placeholder. Both kinds of handle
  are put back here: a const one (`a &gt;&gt; b!`, #5828) and a plain one — an
  abstract formation (`[] &gt;&gt; name`, #5876) or a based one
  (`a.b &gt;&gt; name`, #5944). The single hosting reference is rebuilt from
  the binding above and never reaches this template (`eo:kept` excludes it).
  -->
  <xsl:template match="o[exists(eo:kept-const-ref(.))]/@base" priority="2">
    <xsl:attribute name="base" select="eo:handle-base(.., eo:kept-const-ref(..))"/>
  </xsl:template>
  <!--
  The non-const mirror of the rewrite above.
  -->
  <xsl:template match="o[exists(eo:kept-local-ref(.))]/@base" priority="2">
    <xsl:attribute name="base" select="eo:handle-base(.., eo:kept-local-ref(..))"/>
  </xsl:template>
  <!--
  Drop the standalone binding once it has been merged onto a reference, whether
  the host is a bare/dispatch moniker reference (`eo:moniker-refs`) or an
  applied formation handle folded to a "| args" pipe (`eo:applied-hosted`,
  #5848/#6008). Outranks the merge template above, which matches the same binding
  whenever the binding is a host site itself: what it hosts travels with it to the
  reference in the "merged" mode, so here it only has to go (#5918).
  -->
  <xsl:template match="o[eo:moniker-binding(.) and (exists(eo:moniker-refs(.)) or eo:applied-hosted(.))]" priority="3"/>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
