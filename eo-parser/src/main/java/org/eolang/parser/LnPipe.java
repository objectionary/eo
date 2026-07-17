/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * A pipe-application continuation line — §3.14 of the spec.
 *
 * <p>Form: {@code | [arg…] [> name]}. The {@code |} applies arguments to
 * the <em>same-indent predecessor</em> — the object declared on the lines
 * just above — without naming it at the call site. It is the surface form
 * of phi-calculus formation-with-application {@code ⟦…⟧(…)}: the
 * predecessor is formed, then the pipe supplies its arguments.</p>
 *
 * <p>The predecessor (stack top at the pipe's indent) must be a formation
 * ({@link Kind#BARE_FORMATION} / {@link Kind#ONLY_PHI_FORMATION}) or
 * another {@link Kind#PIPE_APPLICATION}, and must be named — a pipe refers
 * to it by name, so an unnamed formation is not a valid target (R-3.14.2).
 * A pipe after a {@code .method} dispatch is rejected (R-3.14.4): the
 * attribute has already been taken, so the formation is no longer in
 * hand.</p>
 *
 * <p>Two forms by whether horizontal args are present (R-3.14.3): the
 * <em>horizontal</em> form {@code | a b} carries its args on the line and
 * closes for children ({@link Openness#VERTICAL_COMPLETED}); the
 * <em>vertical</em> form {@code |} with a deeper-indent body opens for a
 * vertical-argument block exactly as a {@code vapplication} head, so it
 * stays {@link Openness#OPEN}. Either way a following {@code .method} or
 * pipe may extend it.</p>
 *
 * <p>Emission (R-3.14.7): the line emits a base-less {@code <o pipe=''>}
 * with the args as children; the {@code wrap-applications} reshape sets
 * {@code @base} from the preceding sibling's {@code @name} and drops
 * {@code @pipe}, so downstream passes treat it as a hand-written
 * application of the predecessor by name. The predecessor object stays in
 * place.</p>
 *
 * @since 0.1
 */
final class LnPipe implements Line {

    /**
     * The line's source span.
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The source span
     */
    LnPipe(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        Blanks.checkPlain(this.span, globals, emit);
        this.precheck(stack);
        final Tokens tokens = this.piped();
        final List<Value> args = tokens.readArgs();
        Bindings.checkAllOrNothing(args, this.span);
        final Suffix suffix = new Suffix(
            tokens.tail(), this.span, this.span.indent() + tokens.cursor()
        );
        if (suffix.atom() || suffix.test()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "a pipe application cannot declare an atom or a test attribute"
            );
        }
        Comments.seal(globals, emit, this.span);
        final Openness openness;
        if (args.isEmpty()) {
            openness = Openness.OPEN;
        } else {
            openness = Openness.VERTICAL_COMPLETED;
        }
        new Transition(stack, this.span).apply(
            Kind.PIPE_APPLICATION, openness, suffix.named()
        );
        globals.clearBlanks();
        globals.markEmitted();
        emit.object(
            suffix.attribute(this.span.line(), this.span.indent()),
            null, this.span.line(), this.span.indent()
        );
        emit.pipe();
        if (suffix.constant()) {
            emit.constant();
        }
        for (final Value arg : args) {
            Emissions.emitArg(emit, arg, this.span.line());
        }
    }

    /**
     * Validate the pipe has a legal predecessor — a same-indent
     * formation or pipe that carries a name (R-3.14.2 / R-5.2.4a /
     * R-5.2.5a / R-5.2.11a). An empty stack, a shallower top (descending
     * pipe), an unnamed formation, or a non-pipeable kind (a plain value,
     * an application, or a {@code .method} dispatch) all fail.
     * @param stack Indent stack
     */
    private void precheck(final Stack stack) {
        if (stack.empty()
            || stack.top().indent() != this.span.indent()
            || !stack.top().kind().pipeable()
            || !stack.top().named()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "a pipe must follow a named formation or another pipe"
            );
        }
    }

    /**
     * Build a token stream positioned just past the leading {@code | }
     * marker. The {@code |} must be followed by a single space (before
     * the argument list or suffix).
     * @return Tokens positioned after {@code "| "}
     */
    private Tokens piped() {
        final String body = this.span.body();
        if (body.length() < 2 || body.charAt(1) != ' ') {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "a pipe `|` must be followed by a space"
            );
        }
        final Tokens tokens = new Tokens(body, this.span);
        tokens.seek(1);
        return tokens;
    }
}
