/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * One entry of the indent stack — §5.1 of the spec.
 *
 * <p>Mutable record. Most fields flip after construction as later lines
 * extend the level's expression (e.g., a {@link Kind#HEAD} entry promotes
 * to {@link Kind#VAPPLICATION} once its first deeper child arrives; its
 * {@code openness} progresses {@link Openness#OPEN OPEN} →
 * {@link Openness#VERTICAL_COMPLETED VERTICAL_COMPLETED} when the child
 * block ends).</p>
 *
 * <p>Per the parser-pragmatism rule, this class deliberately holds more
 * than four fields and is mutable in-place: an immutable {@code Level} +
 * copy-on-write would allocate a new object on every line transition,
 * pushing the parser's per-line cost from O(1) to O(D). Mutation is
 * confined to the {@link Stack} that owns this entry; no other class
 * keeps a reference. *
 *
 * @since 0.1
 * @checkstyle MultipleStringLiteralsCheck (200 lines)
 */
@SuppressWarnings("PMD.TooManyMethods")
final class Level {

    /**
     * Indent (spaces) at which this entry's expression starts.
     */
    private final int indent;

    /**
     * Source line on which this entry was first pushed (for error messages).
     */
    private final int start;

    /**
     * Kind of the parent entry — i.e., the entry one position deeper in the
     * stack, or {@link Kind#TOP_LEVEL} for the bottom entry.
     */
    private final Kind parent;

    /**
     * True if the parent entry is itself an atom (a formation with a
     * {@code /sig} suffix). Read by R-5.3.4.
     */
    private final boolean patom;

    /**
     * Current outer kind of this entry's expression. Updates as the
     * expression is extended.
     */
    private Kind kind;

    /**
     * Current openness of this entry. Updates as the entry's block closes
     * or as it absorbs horizontal args.
     */
    private Openness openness;

    /**
     * The source name on this entry's naming line ({@code foo} for
     * {@code > foo}, the handle for {@code >> foo}, empty for a bare
     * {@code >>}), or {@code null} when unnamed. Doubles as the named
     * flag ({@link #named()}) and names the offender in §4.5 errors.
     */
    private String label;

    /**
     * The source name of the only-phi formation this entry argues
     * (empty when anonymous), or {@code null} when it is not such an
     * argument. Set by {@link Stack} (see {@link #argues(String)});
     * doubles as the argument flag ({@link #argument()}) and names the
     * formation in §4.5 errors.
     */
    private String formation;

    /**
     * True if this entry's expression is an atom (formation + {@code /sig}).
     */
    private boolean atom;

    /**
     * For {@link Kind#BARE_REVERSED}: whether the receiver child has been
     * consumed yet.
     */
    private boolean taken;

    /**
     * True once a non-void child has been added under this entry —
     * after which a {@link Kind#VOID} child is rejected (R-3.4.7,
     * voids must precede every other attribute).
     */
    private boolean plain;

    /**
     * For {@link Kind#COMPACT_TUPLE}: the {@code N} count from {@code *N}.
     */
    private int count;

    /**
     * For {@link Kind#COMPACT_TUPLE}: number of deeper-indent children
     * seen so far.
     */
    private int children;

    /**
     * For {@link Kind#COMPACT_TUPLE}: true once the synthesised
     * {@code <o base='Φ.tuple' star=''>} wrapper has been opened for
     * the (N+1)-th and subsequent children.
     */
    private boolean tupled;

    /**
     * For a {@link Kind#ONLY_PHI_FORMATION} whose φ is a compact tuple
     * ({@code seq * > [m]}, R-3.9.1 + R-3.10.6): true so the φ absorbs
     * deeper-indent lines into a {@code Φ.tuple} wrapper like a
     * {@link Kind#COMPACT_TUPLE} head, reusing {@link #count} /
     * {@link #children} / {@link #tupled}.
     */
    private boolean star;

    /**
     * Cross-line binding mode for arg-bearing parents — 0 unset, 1
     * all-unbound, 2 all-bound. Updated by
     * {@link #observeBinding(boolean, Span)} per R-6.6.2.
     */
    private int bindings;

    /**
     * Whether a child arg is currently being tracked but has not yet
     * been committed into {@link #bindings} — see
     * {@link #observeBinding(boolean, Span)} / {@link #commitArg(Span)}.
     */
    private boolean argpending;

    /**
     * Whether the in-progress arg carries a binding (so far). May be
     * flipped to {@code true} mid-chain by
     * {@link #upgradeArgBinding()} when a {@code .method} continuation
     * picks up an outer binding.
     */
    private boolean argbound;

    /**
     * Source span recorded with the in-progress arg, used for error
     * positioning when {@link #commitArg(Span)} rejects the arg
     * against the group mode.
     */
    private Span argspan;

    /**
     * Ctor — fresh level pushed at {@code indent} on {@code line} under
     * {@code parent}.
     * @param ind Indent
     * @param line Start line (1-indexed)
     * @param outer Initial outer kind
     * @param state Initial openness
     * @param parent Parent kind (or {@link Kind#TOP_LEVEL})
     * @param patom Whether the parent entry is itself an atom
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    Level(
        final int ind, final int line, final Kind outer, final Openness state,
        final Kind parent, final boolean patom
    ) {
        this.indent = ind;
        this.start = line;
        this.kind = outer;
        this.openness = state;
        this.parent = parent;
        this.patom = patom;
        this.atom = false;
        this.taken = false;
        this.count = 0;
        this.children = 0;
        this.tupled = false;
        this.star = false;
    }

    /**
     * Indent of this entry.
     * @return Indent
     */
    int indent() {
        return this.indent;
    }

    /**
     * Source line on which the entry was first pushed.
     * @return Start line
     */
    int start() {
        return this.start;
    }

    /**
     * Current outer kind.
     * @return Kind
     */
    Kind kind() {
        return this.kind;
    }

    /**
     * Current openness.
     * @return Openness
     */
    Openness openness() {
        return this.openness;
    }

    /**
     * Parent kind (or {@link Kind#TOP_LEVEL}).
     * @return Parent kind
     */
    Kind parent() {
        return this.parent;
    }

    /**
     * Whether the parent entry is an atom.
     * @return Parent-atom flag
     */
    boolean patom() {
        return this.patom;
    }

    /**
     * Whether this entry has been given a name on its naming line.
     * @return Named flag
     */
    boolean named() {
        return this.label != null;
    }

    /**
     * The name a child of this entry should use for its governing
     * only-phi formation: this entry's own name when it is the
     * {@link Kind#ONLY_PHI_FORMATION}, otherwise the name propagated
     * onto it (see {@link #argues(String)}). Never {@code null} — an
     * anonymous formation propagates as the empty string.
     * @return Governing formation name (possibly empty)
     */
    String governingFormation() {
        final String owner;
        if (this.kind == Kind.ONLY_PHI_FORMATION) {
            owner = this.label;
        } else {
            owner = this.formation;
        }
        final String result;
        if (owner == null) {
            result = "";
        } else {
            result = owner;
        }
        return result;
    }

    /**
     * The §4.5 diagnostic naming the offending attribute and the
     * formation (generic when auto-named / anonymous).
     * @return The error message
     */
    String onlyPhiNamingError() {
        final String owner;
        if (this.formation == null || this.formation.isEmpty()) {
            owner = "an only-phi formation";
        } else {
            owner = String.format("only-phi formation %s", this.formation);
        }
        final String attribute;
        if (this.label == null || this.label.isEmpty()) {
            attribute = "an auto-named attribute";
        } else {
            attribute = this.label;
        }
        return String.format(
            "%s cannot be a named attribute of %s, which binds only its φ decoratee",
            attribute, owner
        );
    }

    /**
     * Whether this entry's expression is itself an atom.
     * @return Atom flag
     */
    boolean atom() {
        return this.atom;
    }

    /**
     * Whether the bare-reversed receiver child has been seen.
     * @return Receiver-consumed flag
     */
    boolean taken() {
        return this.taken;
    }

    /**
     * Whether this entry is an argument of an only-phi formation's φ.
     * @return Argument-position flag
     */
    boolean argument() {
        return this.formation != null;
    }

    /**
     * Whether a child pushed under this entry sits in argument position
     * of an only-phi formation's φ (§4.5): true for a direct child of
     * the only-phi entry, and for a deeper child that stays in argument
     * position — the flag propagates down through nested applications
     * but resets at a formation boundary, where naming resumes.
     * @return True if a child of this entry is an only-phi argument
     */
    boolean argumentative() {
        return this.kind == Kind.ONLY_PHI_FORMATION
            || this.formation != null && !this.kind.formation();
    }

    /**
     * Flag this entry as an argument of an only-phi formation's φ,
     * recording the formation's name for the §4.5 diagnostic.
     * @param owner The formation's name (empty if anonymous, non-null)
     */
    void argues(final String owner) {
        this.formation = owner;
    }

    /**
     * Compact-tuple {@code N} count.
     * @return Compact N
     */
    int count() {
        return this.count;
    }

    /**
     * Deeper-indent child count (for compact-tuple validation).
     * @return Child count
     */
    int children() {
        return this.children;
    }

    /**
     * Mutate the outer kind (e.g., promote {@link Kind#HEAD} →
     * {@link Kind#VAPPLICATION}).
     * @param next New kind
     */
    void become(final Kind next) {
        this.kind = next;
    }

    /**
     * Mutate the openness state.
     * @param next New openness
     */
    void close(final Openness next) {
        this.openness = next;
    }

    /**
     * Record the suffix's source name, which also marks the entry named
     * ({@link #named()}).
     * @param text The name label (empty for a bare {@code >>}); never
     *  {@code null}
     */
    void name(final String text) {
        this.label = text;
    }

    /**
     * Flag this entry as an atom (formation with {@code /sig}).
     */
    void mark() {
        this.atom = true;
    }

    /**
     * Mark the bare-reversed receiver as consumed.
     */
    void consumeReceiver() {
        this.taken = true;
    }

    /**
     * Observe a child of the given {@code kind} being added under this
     * entry, enforcing the void-ordering rule (R-3.4.7): a
     * {@link Kind#VOID} child is rejected once a non-void child has
     * appeared, and every non-void child records that fact.
     * @param shape Kind of the child being added
     * @param line Source line of the child (for the error)
     * @param column Source indent of the child (for the error)
     */
    void observeVoid(final Kind shape, final int line, final int column) {
        if (shape == Kind.VOID && this.plain) {
            throw new ParseError(
                line, column,
                "a void attribute must be declared above all other attributes"
            );
        }
        if (shape != Kind.VOID) {
            this.plain = true;
        }
    }

    /**
     * Set the compact-tuple {@code N} count.
     * @param value N
     */
    void compact(final int value) {
        this.count = value;
    }

    /**
     * Increment the deeper-indent child counter.
     */
    void child() {
        this.children = this.children + 1;
    }

    /**
     * Whether the compact-tuple wrapper has been opened.
     * @return Flag
     */
    boolean tupled() {
        return this.tupled;
    }

    /**
     * Whether this only-phi formation's φ is a compact tuple that
     * absorbs deeper-indent lines as {@code Φ.tuple} elements (R-3.9.1
     * + R-3.10.6).
     * @return Compact-φ flag
     */
    boolean star() {
        return this.star;
    }

    /**
     * Flag this only-phi φ as a compact tuple, so the {@code Φ.tuple}
     * wrapper mechanics apply to its children as for a
     * {@link Kind#COMPACT_TUPLE} head.
     */
    void markStar() {
        this.star = true;
    }

    /**
     * Mark the compact-tuple wrapper as opened — emission of the
     * {@code Φ.tuple} wrapper has fired and any further children land
     * inside it.
     */
    void openTuple() {
        this.tupled = true;
    }

    /**
     * Observe a child arg's binding presence and validate against the
     * group's mode — R-6.6.2. The first observation sets the mode;
     * subsequent ones must match.
     *
     * <p>For vmethod chains (head + same-indent {@code .method}
     * continuations), the binding may live on the last link rather
     * than on the head. This method tracks the *currently in-progress*
     * arg separately and only commits it to the group mode when the
     * next sibling arg starts or the parent closes. A late binding
     * picked up via {@link #upgradeArgBinding()} is reflected at
     * commit time.</p>
     *
     * @param bound Whether the child carries an inline binding
     * @param span Source span of the child (for error positioning)
     */
    void observeBinding(final boolean bound, final Span span) {
        this.commitArg(span);
        this.argpending = true;
        this.argbound = bound;
        this.argspan = span;
    }

    /**
     * Mark the currently in-progress arg as bound — used when a
     * {@code .method} chain continuation carries an outer binding
     * that names the whole expression (binding lives on the last
     * link per the chain-binding rule).
     */
    void upgradeArgBinding() {
        this.argbound = true;
    }

    /**
     * Commit the currently in-progress arg's binding state into the
     * group mode and verify against the all-or-nothing rule. Called
     * before starting a new arg and at parent close time.
     * @param span Span for error positioning when the rule is violated
     */
    void commitArg(final Span span) {
        if (this.argpending) {
            final int code;
            if (this.argbound) {
                code = 2;
            } else {
                code = 1;
            }
            if (this.bindings == 0) {
                this.bindings = code;
            } else if (this.bindings != code) {
                throw new ParseError(
                    this.argspan.line(), this.argspan.indent(),
                    "argument bindings must be all-or-nothing"
                );
            }
            this.argpending = false;
        }
    }
}
