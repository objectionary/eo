/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Xembly emission helper — §9 of the spec.
 *
 * <p>Wraps a growing list of {@link Directive} that downstream
 * {@code Xembler} converts into XMIR. Each emission method appends to the
 * sink in source order; the caller controls when to take a
 * {@link #savepoint()} and how to {@link #rollback(Savepoint)} on a per-line
 * recovery (R-7.2).</p>
 *
 * <p>The class exposes two families of methods:</p>
 *
 * <ul>
 * <li><strong>Side-panel</strong> ({@link #meta}, {@link #comment},
 * {@link #error}) — append directives to fixed paths under
 * {@code /object} (metas, comments, errors). These wrap their
 * navigation in {@code push}/{@code pop} so the caller's cursor
 * position survives the call. Safe to invoke at any depth.</li>
 * <li><strong>Object tree</strong> ({@link #object}, {@link #unnamedObject},
 * {@link #baselessObject}, {@link #bareObject}, {@link #close},
 * {@link #voidParam}, {@link #atomMarker}) — emit relative to the
 * current cursor, descending into the new {@code <o>} on
 * {@link #object} and ascending on {@link #close}. The caller must
 * open the {@code <object>} root and position the cursor inside it
 * before invoking these.</li>
 * </ul>
 *
 * @since 0.1
 */
final class Emit {

    /**
     * Flat list of directives, appended in source order.
     */
    private final List<Directive> sink;

    /**
     * Source lines for caret-underlined error messages (1-indexed
     * lookup). Empty when no source is wired through — in that case
     * {@link #error} falls back to the bare {@code [L:P] message} form.
     */
    private final Rows lines;

    /**
     * The atom signature owed to an open {@code <o>} as its {@code λ}
     * child, empty when nothing is owed. An atom carries its signature
     * on the head line but declares its voids below it, and every void
     * belongs ahead of the marker (§9.4), so the marker waits here
     * until the object it belongs to takes a child that is not a void,
     * or closes with none.
     */
    private String signature;

    /**
     * Source line of the owed marker.
     */
    private int sigline;

    /**
     * Source column of the owed marker.
     */
    private int sigpos;

    /**
     * Depth at which the object that owes the marker sits.
     */
    private int sigdepth;

    /**
     * How many {@code <o>} elements stand open at the cursor.
     */
    private int depth;

    /**
     * Ctor.
     */
    Emit() {
        this(List.of());
    }

    /**
     * Primary ctor.
     * @param source The source lines {@link Source} split off the text
     *  being parsed (for caret-underlined error messages — pass an
     *  empty list to disable)
     */
    Emit(final List<Span> source) {
        this.signature = "";
        this.sink = new ArrayList<>(0);
        this.lines = new Rows(source);
    }

    /**
     * Take a savepoint of the emitter's cursor: sink size, {@link #depth},
     * and any atom signature owed to an open {@code <o>} — R-7.2. All
     * three must roll back together via {@link #rollback(Savepoint)}, or
     * an error recovered mid-formation leaves {@link #depth} drifted from
     * the tree it describes (#7539).
     * @return Savepoint token
     */
    Savepoint savepoint() {
        return new Savepoint(
            this.sink.size(), this.depth, this.signature,
            this.sigline, this.sigpos, this.sigdepth
        );
    }

    /**
     * Roll the sink and cursor back to the given savepoint, discarding
     * any directives appended after it and restoring {@link #depth} and
     * the owed atom signature to what they were at that point.
     * @param token Savepoint token from {@link #savepoint()}
     */
    void rollback(final Savepoint token) {
        while (this.sink.size() > token.sink()) {
            this.sink.remove(this.sink.size() - 1);
        }
        token.restore(this);
    }

    /**
     * Put {@link #depth} back to what it was at a savepoint.
     * @param cursor Depth to restore
     */
    void depth(final int cursor) {
        this.depth = cursor;
    }

    /**
     * Put the owed atom signature back to what it was at a savepoint.
     * @param owed Signature owed to the open object, empty when nothing is owed
     * @param line Source line of the owed marker
     * @param pos Source column of the owed marker
     * @param level Depth of the object owing the marker
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    void signature(final String owed, final int line, final int pos, final int level) {
        this.signature = owed;
        this.sigline = line;
        this.sigpos = pos;
        this.sigdepth = level;
    }

    /**
     * Append a meta directive to {@code /object/metas} — §3.2 / §9.4.
     *
     * <p>Wraps absolute navigation in {@code push}/{@code pop} so the
     * caller's cursor position is preserved.</p>
     *
     * @param line Source line of the meta
     * @param head Name after the {@code +}
     * @param parts Space-separated tokens following the head
     */
    void meta(final int line, final String head, final List<String> parts) {
        final Directives dirs = new Directives()
            .push()
            .xpath("/object")
            .strict(1)
            .addIf("metas")
            .strict(1)
            .add("meta")
            .attr("line", line)
            .add("head").set(head).up()
            .add("tail");
        if (parts.isEmpty()) {
            dirs.up();
        } else {
            dirs.set(String.join(" ", parts)).up();
            for (final String part : parts) {
                dirs.add("part").set(part).up();
            }
        }
        dirs.up().up().pop();
        this.append(dirs);
    }

    /**
     * Append a comment directive to {@code /object/comments} — §9.5.
     *
     * <p>The body is the comment text with the leading {@code #} stripped
     * from each line. If the comment block spans multiple lines, they
     * are joined by {@code \n}. The reported line number is the caller's
     * choice of target line. Wraps absolute navigation in
     * {@code push}/{@code pop}.</p>
     *
     * @param spans Comment line spans, in source order
     * @param target Line to report for the flushed comment
     */
    void comment(final List<Span> spans, final int target) {
        if (spans.isEmpty()) {
            return;
        }
        final StringBuilder body = new StringBuilder(spans.size() * 32);
        for (int idx = 0; idx < spans.size(); idx = idx + 1) {
            final String text = spans.get(idx).body();
            final String stripped;
            if (text.startsWith("# ")) {
                stripped = text.substring(2);
            } else if (text.startsWith("#")) {
                stripped = text.substring(1);
            } else {
                stripped = text;
            }
            if (idx > 0) {
                body.append('\n');
            }
            body.append(stripped);
        }
        this.append(
            new Directives()
                .push()
                .xpath("/object")
                .strict(1)
                .addIf("comments")
                .strict(1)
                .add("comment")
                .attr("line", target)
                .set(new Scrubbed(body.toString()))
                .up().up()
                .pop()
        );
    }

    /**
     * Append a non-lossy error directive to {@code /object/errors} — see
     * {@link #error(int, int, String, boolean)}.
     * @param line Line where the error occurred
     * @param pos Column where the error occurred (0-indexed)
     * @param message Canonical message text (no position prefix)
     */
    void error(final int line, final int pos, final String message) {
        this.error(line, pos, message, false);
    }

    /**
     * Emit a parser {@code <error>} at the current line/position (R-9.9.1 /
     * R-9.1.2), and the canonical message text from §9.9 prefixed with
     * {@code [L:P]} per R-9.9.2. Wraps absolute navigation in
     * {@code push}/{@code pop}. The {@code check} attribute is always set
     * to {@code "parser"}, since this is a plain parser syntax error, not
     * one raised by a named lint rule (#6215): downstream code (see
     * {@code Linting.toDefect} in eo-maven-plugin) fails fast if
     * {@code check} is ever missing, so it must be set here rather than
     * defaulted downstream.
     * @param line Line where the error occurred
     * @param pos Column where the error occurred (0-indexed)
     * @param message Canonical message text (no position prefix)
     * @param lossy Whether recovering from this error dropped the whole
     *  construct it was building, rather than merely flagging a cosmetic
     *  or informational issue with a construct that still made it into
     *  the tree intact (#6649)
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    void error(final int line, final int pos, final String message, final boolean lossy) {
        final Directives dirs = new Directives()
            .push()
            .xpath("/object")
            .strict(1)
            .addIf("errors")
            .strict(1)
            .add("error")
            .attr("line", line)
            .attr("pos", pos)
            .attr("check", "parser")
            .attr("severity", "error");
        if (lossy) {
            dirs.attr("lossy", "");
        }
        this.append(
            dirs.set(new Scrubbed(this.lines.underlined(line, pos, message)))
                .up().up()
                .pop()
        );
    }

    /**
     * Open an {@code <o base="...">} element at the current cursor —
     * §9.0.2 / §9.4.2.
     *
     * <p>The cursor descends into the new element so subsequent
     * {@link #voidParam}, {@link #atomMarker}, and child {@link #object}
     * calls add as children. Must be balanced by {@link #close()} when
     * the element's body ends.</p>
     *
     * @param name Value for {@code @name}, or {@code null} to omit
     * @param base Value for {@code @base}
     * @param line Source line for {@code @line}
     * @param pos Source column for {@code @pos}
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    void object(
        final String name, final String base, final int line, final int pos
    ) {
        this.open(name, base, line, pos);
    }

    /**
     * Open an unnamed {@code <o base="...">} element, i.e. one with
     * no {@code @name} — §9.0.2 / §9.4.2.
     * @param base Value for {@code @base}
     * @param line Source line for {@code @line}
     * @param pos Source column for {@code @pos}
     */
    void unnamedObject(final String base, final int line, final int pos) {
        this.open(null, base, line, pos);
    }

    /**
     * Open a named, base-less {@code <o>} element — one whose
     * {@code @base} is absent because the {@code wrap-applications}
     * reshape fills it in later — §9.0.2 / §9.4.2.
     * @param name Value for {@code @name}, or {@code null} to omit
     * @param line Source line for {@code @line}
     * @param pos Source column for {@code @pos}
     */
    void baselessObject(final String name, final int line, final int pos) {
        this.open(name, null, line, pos);
    }

    /**
     * Open an unnamed, base-less {@code <o>} element — neither
     * {@code @name} nor {@code @base} is written — §9.0.2 / §9.4.2.
     * @param line Source line for {@code @line}
     * @param pos Source column for {@code @pos}
     */
    void bareObject(final int line, final int pos) {
        this.open(null, null, line, pos);
    }

    /**
     * Add the {@code @const} attribute to the most recently opened
     * {@code <o>} (R-9.4 const-marker).
     */
    void constant() {
        this.append(new Directives().attr("const", ""));
    }

    /**
     * Add the {@code @method=""} attribute to the most recently opened
     * {@code <o>} (R-9.4 method-link marker per §9.0.3).
     */
    void method() {
        this.append(new Directives().attr("method", ""));
    }

    /**
     * Add the {@code @method=""} attribute to the most recently opened
     * {@code <o>}, plus {@code @fragile=""} when the dispatch is the
     * fragile {@code ?.} operator (R-3.5 / §9.4).
     * @param fragile Whether the dispatch link is fragile ({@code ?.})
     */
    void method(final boolean fragile) {
        this.method();
        if (fragile) {
            this.fragile();
        }
    }

    /**
     * Add the {@code @fragile=""} attribute to the most recently opened
     * {@code <o>} — marks a {@code ?.} fragile dispatch (R-3.5 / §9.4).
     * A reversed dispatch carries it without {@code @method}; a method
     * link carries both (see {@link #method(boolean)}).
     */
    void fragile() {
        this.append(new Directives().attr("fragile", ""));
    }

    /**
     * Add the {@code @pipe=""} attribute to the most recently opened
     * {@code <o>} — marks a base-less pipe-application node (§3.14 /
     * §9.4) that the {@code wrap-applications} reshape rewrites into an
     * application referring to the preceding sibling.
     */
    void pipe() {
        this.append(new Directives().attr("pipe", ""));
    }

    /**
     * Add the {@code @star=""} attribute to the most recently opened
     * {@code <o>} (§9.4 compact-tuple wrapper marker and §9.4.2 star
     * head).
     */
    void star() {
        this.append(new Directives().attr("star", ""));
    }

    /**
     * Add the {@code @local="name"} attribute to the most recently
     * opened {@code <o>} — the file-local handle of an anonymous
     * ({@code >> name}) formation (§3.10 / §9.2). The
     * {@code resolve-local-names} reshape uses it to resolve references
     * and keeps it on the declaring object so the readable handle can be
     * recovered later (for example by the printer, see #5563).
     * @param name The file-local handle
     */
    void local(final String name) {
        this.append(new Directives().attr("local", name));
    }

    /**
     * Add the {@code @args="type …"} attribute to the most recently
     * opened {@code <o>} — the space-separated argument-type list of an
     * atom's vertical void callback branch (R-3.4.8). Each token is a
     * concrete forma (homed by later passes) or a verbatim generic type
     * variable.
     * @param types Space-separated argument-type list
     */
    void args(final String types) {
        this.append(new Directives().attr("args", types));
    }

    /**
     * Add the {@code @type="type"} attribute to the most recently opened
     * {@code <o>} — the declared type of an atom's vertical void
     * attribute (R-3.4.8): a concrete forma or a generic type variable,
     * with an optional trailing {@code ?} marking a maybe-terminator value.
     * @param type The declared type
     */
    void type(final String type) {
        this.append(new Directives().attr("type", type));
    }

    /**
     * Add the {@code @as=tag} attribute to the most recently opened
     * {@code <o>} — inline-binding marker per §3.12 / §9.4. Numeric
     * tags must be pre-formatted as {@code αN} by the caller.
     * @param tag Binding tag
     */
    void slot(final String tag) {
        this.append(new Directives().attr("as", tag));
    }

    /**
     * Set the text content of the most recently opened {@code <o>}.
     * Used for {@code <o base='Φ.bytes'>HEX</o>} value carriers (R-9.4
     * data carriers).
     * @param text Text content
     */
    void set(final String text) {
        this.append(new Directives().set(text));
    }

    /**
     * Close the most recently opened {@code <o>} — moves the cursor up
     * one level. Must balance a prior {@link #object} call.
     */
    void close() {
        this.lambda();
        this.depth = this.depth - 1;
        this.append(new Directives().up());
    }

    /**
     * Emit a void parameter child — an {@code <o>} named after the
     * parameter and based on the empty set, per §9.4. The cursor is
     * expected to be inside the parent formation's {@code <o>}.
     * @param name Parameter name
     * @param line Source line of the formation
     * @param pos Source column of the parameter
     */
    void voidParam(final String name, final int line, final int pos) {
        this.append(
            new Directives()
                .add("o")
                .attr("name", name)
                .attr("base", "∅")
                .attr("line", line)
                .attr("pos", pos)
                .up()
        );
    }

    /**
     * Emit the atom marker child for a formation declared with
     * {@code /sig} — {@code <o name='λ' atom='<sig>'/>} per §9.4. The
     * cursor is expected to be inside the parent atom's {@code <o>}.
     * @param sig Atom signature value (already promoted from Q to Φ)
     * @param line Source line of the atom declaration
     * @param pos Source column of the {@code /sig} marker
     */
    void atomMarker(final String sig, final int line, final int pos) {
        this.signature = sig;
        this.sigline = line;
        this.sigpos = pos;
        this.sigdepth = this.depth;
    }

    /**
     * Materialise all accumulated directives.
     *
     * <p>The returned iterable is the same list the emission methods
     * append to. Iteration order matches source order.</p>
     *
     * @return Directives in append order
     */
    Iterable<Directive> directives() {
        return this.sink;
    }

    private void open(
        final String name, final String base, final int line, final int pos
    ) {
        final Directives dirs = new Directives().add("o");
        if (name != null) {
            dirs.attr("name", name);
        }
        if (base != null) {
            dirs.attr("base", base);
        }
        dirs.attr("line", line).attr("pos", pos);
        if (!"∅".equals(base)) {
            this.lambda();
        }
        this.append(dirs);
        this.depth = this.depth + 1;
    }

    private void lambda() {
        if (!this.signature.isEmpty() && this.depth == this.sigdepth) {
            this.append(
                new Directives()
                    .add("o")
                    .attr("name", "λ")
                    .attr("atom", this.signature)
                    .attr("line", this.sigline)
                    .attr("pos", this.sigpos)
                    .up()
            );
            this.signature = "";
        }
    }

    private void append(final Iterable<Directive> dirs) {
        final Iterator<Directive> iterator = dirs.iterator();
        while (iterator.hasNext()) {
            this.sink.add(iterator.next());
        }
    }
}
