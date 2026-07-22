/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Xembly emission helper — §9 of the spec.
 *
 * <p>Wraps a growing list of {@link Directive} that downstream
 * {@code Xembler} converts into XMIR. Each emission method appends to the
 * sink in source order; the caller controls when to take a
 * {@link #savepoint()} and how to {@link #rollback(int)} on a per-line
 * recovery (R-7.2).</p>
 *
 * <p>The class exposes two families of methods:</p>
 *
 * <ul>
 *   <li><strong>Side-panel</strong> ({@link #meta}, {@link #comment},
 *   {@link #error}) — append directives to fixed paths under
 *   {@code /object} (metas, comments, errors). These wrap their
 *   navigation in {@code push}/{@code pop} so the caller's cursor
 *   position survives the call. Safe to invoke at any depth.</li>
 *   <li><strong>Object tree</strong> ({@link #object}, {@link #close},
 *   {@link #voidParam}, {@link #atomMarker}) — emit relative to the
 *   current cursor, descending into the new {@code <o>} on
 *   {@link #object} and ascending on {@link #close}. The caller must
 *   open the {@code <object>} root and position the cursor inside it
 *   before invoking these.</li>
 * </ul>
 *
 * @since 0.1
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.AvoidDuplicateLiterals", "PMD.UnnecessaryLocalRule"})
final class Emit {

    /**
     * Pre-compiled line-break splitter for the source text passed to
     * the {@link #Emit(String)} constructor.
     */
    private static final Pattern EOL = Pattern.compile("\\R");

    /**
     * Flat list of directives, appended in source order.
     */
    private final List<Directive> sink;

    /**
     * Source lines for caret-underlined error messages (1-indexed
     * lookup). Empty when no source is wired through — in that case
     * {@link #error} falls back to the bare {@code [L:P] message} form.
     */
    private final List<String> lines;

    /**
     * Ctor.
     * @checkstyle ConstructorsCodeFreeCheck (3 lines)
     */
    Emit() {
        this(List.of());
    }

    /**
     * Ctor.
     * @param source Raw EO source text (for caret-underlined error
     *  messages — pass empty string to disable)
     * @checkstyle ConstructorsCodeFreeCheck (3 lines)
     */
    Emit(final String source) {
        this(List.of(Emit.EOL.split(source, -1)));
    }

    /**
     * Primary ctor.
     * @param src Pre-split source lines
     */
    private Emit(final List<String> src) {
        this.sink = new LinkedList<>();
        this.lines = src;
    }

    /**
     * Take a savepoint of the current sink size.
     *
     * <p>R-7.2 — the parser takes one of these at the start of each line.
     * On a parse error inside that line, the caller invokes
     * {@link #rollback(int)} with this token to drop the line's
     * half-built directives.</p>
     *
     * @return Savepoint token (current sink size)
     */
    int savepoint() {
        return this.sink.size();
    }

    /**
     * Roll the sink back to the given savepoint, discarding any
     * directives appended after it.
     * @param token Savepoint token from {@link #savepoint()}
     */
    void rollback(final int token) {
        while (this.sink.size() > token) {
            this.sink.remove(this.sink.size() - 1);
        }
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
     * are joined by {@code \n}. The reported line number is the line of
     * the <em>last</em> comment line in the block. Wraps absolute
     * navigation in {@code push}/{@code pop}.</p>
     *
     * @param spans Comment line spans, in source order
     * @param target Line of the named object the comment attaches to
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
        final Directives dirs = new Directives()
            .push()
            .xpath("/object")
            .strict(1)
            .addIf("comments")
            .strict(1)
            .add("comment")
            .attr("line", target)
            .set(body.toString())
            .up().up()
            .pop();
        this.append(dirs);
    }

    /**
     * Append an error directive to {@code /object/errors} — §9.6.
     *
     * <p>Records the {@code line}, {@code pos} (0-indexed column per
     * R-9.1.2), and the canonical message text from §9.9 prefixed with
     * {@code [L:P]} per R-9.9.2. Wraps absolute navigation in
     * {@code push}/{@code pop}.</p>
     *
     * @param line Line where the error occurred
     * @param pos Column where the error occurred (0-indexed)
     * @param message Canonical message text (no position prefix)
     */
    void error(final int line, final int pos, final String message) {
        final Directives dirs = new Directives()
            .push()
            .xpath("/object")
            .strict(1)
            .addIf("errors")
            .strict(1)
            .add("error")
            .attr("line", line)
            .attr("pos", pos)
            .attr("severity", "error")
            .set(this.formatted(line, pos, message))
            .up().up()
            .pop();
        this.append(dirs);
    }

    /**
     * Open an {@code <o>} element at the current cursor — §9.0.2 /
     * §9.4.2.
     *
     * <p>The cursor descends into the new element so subsequent
     * {@link #voidParam}, {@link #atomMarker}, and child {@link #object}
     * calls add as children. Must be balanced by {@link #close()} when
     * the element's body ends.</p>
     *
     * <p>{@code name} and {@code base} may be {@code null} (omitted on
     * the emitted element). {@code line} and {@code pos} are written as
     * {@code @line} / {@code @pos} per R-9.1.</p>
     *
     * @param name Value for {@code @name}, or {@code null} to omit
     * @param base Value for {@code @base}, or {@code null} to omit
     * @param line Source line for {@code @line}
     * @param pos Source column for {@code @pos}
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    void object(
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
        this.append(dirs);
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
     * Add the {@code @self=""} attribute to the most recently opened
     * {@code <o>} — marks a base-less {@code %} self-reference node
     * (§3.15 / §9.4) whose {@code @base} the {@code resolve-self} reshape
     * fills in with the enclosing anonymous formation's auto-name.
     */
    void self() {
        this.append(new Directives().attr("self", ""));
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
     * with an optional trailing {@code ?} marking a maybe-⊥ value.
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
     * Used for {@code <o base='Φ.bytes'>HEX&lt;/o>} value carriers (R-9.4
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
        this.append(new Directives().up());
    }

    /**
     * Emit a void parameter child — {@code <o name='<param>' base='∅'/>}
     * per §9.4. The cursor is expected to be inside the parent
     * formation's {@code <o>}.
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
     * @param sig Atom signature value (already Q→Φ promoted)
     * @param line Source line of the atom declaration
     * @param pos Source column of the {@code /sig} marker
     */
    void atomMarker(final String sig, final int line, final int pos) {
        this.append(
            new Directives()
                .add("o")
                .attr("name", "λ")
                .attr("atom", sig)
                .attr("line", line)
                .attr("pos", pos)
                .up()
        );
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

    /**
     * Append a directives iterable to the sink.
     * @param dirs Directives to append
     */
    private void append(final Iterable<Directive> dirs) {
        final Iterator<Directive> iterator = dirs.iterator();
        while (iterator.hasNext()) {
            this.sink.add(iterator.next());
        }
    }

    /**
     * Render the error text in the canonical
     * {@code [L:P] error: 'message'} form, followed by the offending
     * source line and a caret pointer when source is available.
     * @param line Line number (1-indexed)
     * @param pos Column (0-indexed)
     * @param message The bare message
     * @return Formatted error text
     */
    private String formatted(final int line, final int pos, final String message) {
        final String located = new MsgLocated(line, pos, message).formatted();
        final String result;
        if (line >= 1 && line <= this.lines.size()) {
            final String source = this.lines.get(line - 1);
            result = String.format(
                "%s%n%s",
                located,
                new MsgUnderlined(source, pos, 1).formatted()
            );
        } else {
            result = located;
        }
        return result;
    }
}
