/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Set;

/**
 * A parsed name suffix — §3.10 of the spec.
 *
 * <p>Recognises the four base forms (mutually exclusive on any line)
 * plus the {@code !} const modifier and the {@code /sig} atom signature:
 * </p>
 *
 * <ul>
 *   <li>{@code > name} — explicit name binding.</li>
 *   <li>{@code >>} — auto-generated name, optional handle (§3.10).</li>
 *   <li>{@code +> name} — truthy test attribute.</li>
 *   <li>{@code -> name} — throwing test attribute (expected to throw).</li>
 *   <li>(empty) — no suffix.</li>
 * </ul>
 *
 * <p>The composite only-phi forms ({@code > [params] > name}) are
 * <em>not</em> parsed here; those are an only-phi-formation line
 * shape (§4.5) with its own classifier. This class handles only the
 * suffix portion of an already-classified non-only-phi line.</p>
 *
 * <p>Validation enforced at construction time:</p>
 *
 * <ul>
 *   <li>R-3.10.2 — {@code >>} cannot carry {@code /sig}.</li>
 *   <li>R-3.10.3 — {@code > name!} cannot combine with {@code /sig}.</li>
 *   <li>R-3.10.10 — {@code /sig} must be a non-empty dotted name
 *   (optionally rooted at {@code Q}) or a generic type variable
 *   {@code A}–{@code F}; a bare {@code /}, {@code /Q} alone, or a
 *   {@code ?} optional marker (void-only) is rejected.</li>
 *   <li>R-6.3.5 — a {@code +>} test name must be a {@code NAME} token,
 *   not {@code @} (PHI).</li>
 * </ul>
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.TooManyMethods")
final class Suffix {

    /**
     * Additional NAME-only token boundaries beyond {@link #terminates}.
     * §2.3 forbids these inside a NAME; signatures keep the loose
     * {@link #terminates} so dotted FQNs are still consumed whole.
     */
    private static final String NAME_BOUNDARIES = ",.|':;?[]{}()";

    /**
     * Scope symbols that cannot be used as file-local handles.
     */
    private static final Set<String> SCOPES = Set.of("@", "^", "$");

    /**
     * Suffix form.
     */
    private final Form form;

    /**
     * Bound name for {@code NAME} / {@code TEST} forms; the file-local
     * handle for a {@code >> name} {@code AUTO} suffix; empty otherwise.
     */
    private final String label;

    /**
     * Atom signature for {@code /sig}; empty if absent.
     */
    private final String sig;

    /**
     * True if the suffix carries the {@code !} const marker.
     */
    private final boolean constant;

    /**
     * Ctor — parses the given tail.
     *
     * <p>{@code tail} is the substring of the source line that follows
     * the head expression. {@code span} is the containing line for
     * error position reporting. {@code home} is the column in the
     * source line at which {@code tail} begins.</p>
     *
     * @param tail Tail substring (may have leading whitespace)
     * @param span Source span (for error reporting)
     * @param home Source column where {@code tail} begins
     * @checkstyle ConstructorsCodeFreeCheck (3 lines)
     */
    Suffix(final String tail, final Span span, final int home) {
        this(Suffix.parse(tail, span, home));
    }

    /**
     * Primary ctor — copies fields from a parsed result.
     * @param result Parsed result
     */
    private Suffix(final Parsed result) {
        this.form = result.form;
        this.label = result.label;
        this.sig = result.sig;
        this.constant = result.constant;
    }

    /**
     * The suffix form — one of {@code NONE}, {@code NAME}, {@code AUTO},
     * {@code TEST}.
     * @return Form
     */
    Form form() {
        return this.form;
    }

    /**
     * Bound name. Empty for {@code AUTO} and {@code NONE}.
     * @return Name
     */
    String label() {
        return this.label;
    }

    /**
     * The suffix's source name, or {@code null} when no suffix is
     * present — distinguishing a bare {@code >>} (present, empty handle)
     * from no suffix, so a caller can mark a level named yet record an
     * empty display name.
     * @return Source name (possibly empty), or {@code null}
     */
    String named() {
        final String result;
        if (this.present()) {
            result = this.label;
        } else {
            result = null;
        }
        return result;
    }

    /**
     * Atom signature. Empty if no {@code /sig} was present.
     * @return Signature, with leading {@code Q} promoted to {@code Φ}
     */
    String sig() {
        return this.sig;
    }

    /**
     * Whether the {@code !} const marker is present.
     * @return Const flag
     */
    boolean constant() {
        return this.constant;
    }

    /**
     * Resolve the {@code @name} attribute value for the line carrying
     * this suffix, applying R-9.3 source-token mapping: {@code @} →
     * {@code φ} for an explicit name.
     *
     * <p>This is the single source of truth for naming any line shape
     * — formations, applications, method chains, reversed dispatches,
     * compact tuples, only-phi formations, text blocks. Returns
     * {@code null} for {@link Form#NONE} (no name attribute).</p>
     *
     * @param line Source line (for {@link Form#AUTO} naming)
     * @param indent Source indent (for {@link Form#AUTO} naming)
     * @return The {@code @name} value, or {@code null}
     */
    String attribute(final int line, final int indent) {
        final String name;
        if (this.form == Form.NAME) {
            name = Suffix.phi(this.label);
        } else if (this.form == Form.TEST) {
            name = "+".concat(this.label);
        } else if (this.form == Form.THROWS) {
            name = "-".concat(this.label);
        } else if (this.form == Form.AUTO) {
            name = new AutoName(line, indent).asString();
        } else {
            name = null;
        }
        return name;
    }

    /**
     * Whether this suffix declares an atom (carries a non-empty
     * {@code /sig}).
     * @return Atom flag
     */
    boolean atom() {
        return !this.sig.isEmpty();
    }

    /**
     * Whether this suffix is a test attribute — either a truthy
     * {@code +> name} or a throwing {@code -> name}.
     * @return Test flag
     */
    boolean test() {
        return this.form == Form.TEST || this.form == Form.THROWS;
    }

    /**
     * Whether this suffix is an auto-generated name ({@code >>}).
     * @return Auto flag
     */
    boolean auto() {
        return this.form == Form.AUTO;
    }

    /**
     * The file-local handle carried by a {@code >> name} auto suffix
     * (§3.10). Empty for a bare {@code >>} and every non-auto form.
     * @return Handle name, or empty string
     */
    String handle() {
        final String result;
        if (this.form == Form.AUTO) {
            result = this.label;
        } else {
            result = "";
        }
        return result;
    }

    /**
     * Whether any suffix is present (form is not {@code NONE}).
     * @return Present flag
     */
    boolean present() {
        return this.form != Form.NONE;
    }

    /**
     * Classify and promote a single type atom — a generic type variable
     * or a concrete forma — shared by an atom return signature
     * (§3.10.10) and a void type annotation (§3.4.8, {@link LnVoid}).
     *
     * <p>A single uppercase letter {@code A}–{@code F} is a generic type
     * variable, returned verbatim so that no later pass homes it into
     * {@code Φ}. A {@code Q.}-rooted forma is promoted to {@code Φ.}
     * (R-9.3). Any other uppercase-initial token is a malformed variable
     * and rejected. Every other token is a concrete forma, returned
     * verbatim for {@code add-default-package} to home.</p>
     *
     * @param raw Raw token, without a trailing {@code ?}
     * @param span Source span
     * @param pos Source column of the token (for errors)
     * @return Emitted token — variable verbatim, forma promoted
     */
    static String typeAtom(final String raw, final Span span, final int pos) {
        final char first = raw.charAt(0);
        if (first >= 'A' && first <= 'Z' && !raw.matches("[A-F]") && !raw.startsWith("Q.")) {
            throw new ParseError(
                span.line(), pos,
                "type variable must be one of A-F"
            );
        }
        final String promoted;
        if (raw.startsWith("Q.")) {
            promoted = "Φ".concat(raw.substring(1));
        } else {
            promoted = raw;
        }
        return promoted;
    }

    /**
     * Apply the R-9.3.1 source-token mapping {@code @} → {@code φ}
     * for a name carried by an explicit {@code > name} suffix. Other
     * names pass through unchanged.
     * @param raw Source name
     * @return Mapped XMIR name
     */
    private static String phi(final String raw) {
        final String mapped;
        if ("@".equals(raw)) {
            mapped = "φ";
        } else {
            mapped = raw;
        }
        return mapped;
    }

    /**
     * Parse a suffix tail into a result struct.
     * @param tail Tail substring
     * @param span Source span
     * @param home Source column where {@code tail} begins
     * @return Parsed result
     */
    private static Parsed parse(final String tail, final Span span, final int home) {
        final int idx = Suffix.start(tail);
        final Parsed result;
        if (idx >= tail.length()) {
            result = new Suffix.Parsed(Form.NONE, "", "", false);
        } else if (tail.startsWith("+>", idx)) {
            result = Suffix.test(tail, idx + 2, span, home, Form.TEST);
        } else if (tail.startsWith("->", idx)) {
            result = Suffix.test(tail, idx + 2, span, home, Form.THROWS);
        } else if (tail.startsWith(">>", idx)) {
            result = Suffix.auto(tail, idx + 2, span, home);
        } else if (tail.charAt(idx) == '>') {
            result = Suffix.named(tail, idx + 1, span, home);
        } else {
            throw new ParseError(
                span.line(), home + idx,
                "trailing garbage after expression"
            );
        }
        return result;
    }

    /**
     * Find the first non-space character in a suffix tail.
     * @param tail Tail substring
     * @return First non-space index, or the tail length
     */
    private static int start(final String tail) {
        int idx = 0;
        while (idx < tail.length() && tail.charAt(idx) == ' ') {
            idx = idx + 1;
        }
        return idx;
    }

    /**
     * Parse a test-attribute suffix — the truthy {@code +> name}
     * ({@link Form#TEST}) or the throwing {@code -> name}
     * ({@link Form#THROWS}). Both share the same {@code NAME}-token
     * grammar; only the resulting form and marker prefix differ.
     * @param tail Tail substring
     * @param after Index immediately after the two-character marker
     * @param span Source span
     * @param home Source column where tail begins
     * @param form The form to record ({@code TEST} or {@code THROWS})
     * @return Parsed result
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private static Parsed test(
        final String tail, final int after, final Span span, final int home, final Form form
    ) {
        int idx = Suffix.skipSpace(tail, after);
        if (idx < tail.length() && tail.charAt(idx) == '@') {
            throw new ParseError(
                span.line(), home + idx,
                "test attribute name must be an identifier, not @"
            );
        }
        final int start = idx;
        while (idx < tail.length() && !Suffix.endsName(tail.charAt(idx))) {
            idx = idx + 1;
        }
        if (start == idx) {
            throw new ParseError(
                span.line(), home + start,
                "test attribute requires a name"
            );
        }
        final String name = tail.substring(start, idx);
        if (name.codePoints().anyMatch(cp -> cp == 0x1F335)) {
            throw new ParseError(
                span.line(), home + start,
                "cactus emoji is reserved for auto-names; not allowed in identifiers"
            );
        }
        Suffix.endsClean(tail, idx, span, home);
        return new Suffix.Parsed(form, name, "", false);
    }

    /**
     * Parse a {@code >>} (auto) suffix, optionally carrying a trailing
     * file-local handle ({@code >> name}, §3.10) kept as the label, and a
     * {@code !} const marker (R-9.4) either right after {@code >>}
     * ({@code >>!}) or after the handle ({@code >> name!}, #5817).
     * @param tail Tail substring
     * @param after Index immediately after {@code >>}
     * @param span Source span
     * @param home Source column where tail begins
     * @return Parsed result
     * @checkstyle ParameterNumberCheck (5 lines)
     * @checkstyle CyclomaticComplexityCheck (45 lines)
     * @checkstyle NPathComplexityCheck (45 lines)
     */
    private static Parsed auto(
        final String tail, final int after, final Span span, final int home
    ) {
        int idx = after;
        boolean cnst = false;
        if (idx < tail.length() && tail.charAt(idx) == '!') {
            cnst = true;
            idx = idx + 1;
        }
        final int begin = Suffix.skipSpace(tail, idx);
        String handle = "";
        int rest = begin;
        if (begin < tail.length() && !Suffix.endsName(tail.charAt(begin))) {
            int end = begin;
            while (end < tail.length() && !Suffix.endsName(tail.charAt(end))) {
                end = end + 1;
            }
            handle = tail.substring(begin, end);
            Suffix.rejectScope(handle, span, home + begin);
            if (handle.codePoints().anyMatch(cp -> cp == 0x1F335)) {
                throw new ParseError(
                    span.line(), home + begin,
                    "cactus emoji is reserved for auto-names; not allowed in identifiers"
                );
            }
            rest = end;
        }
        if (!cnst && rest < tail.length() && tail.charAt(rest) == '!') {
            cnst = true;
            rest = rest + 1;
        }
        final int trailing = Suffix.skipSpace(tail, rest);
        if (trailing < tail.length() && tail.charAt(trailing) == '/') {
            throw new ParseError(
                span.line(), home + trailing,
                "auto-named atom is forbidden"
            );
        }
        Suffix.endsClean(tail, trailing, span, home);
        return new Suffix.Parsed(Form.AUTO, handle, "", cnst);
    }

    /**
     * Reject a scope symbol used as a file-local handle.
     * @param handle File-local handle
     * @param span Source span
     * @param column Source column of the handle
     */
    private static void rejectScope(
        final String handle, final Span span, final int column
    ) {
        if (Suffix.SCOPES.contains(handle)) {
            throw new ParseError(
                span.line(), column,
                "file-local handle must be an identifier, not a scope symbol"
            );
        }
    }

    /**
     * Parse a {@code > name} suffix.
     * @param tail Tail substring
     * @param from Index immediately after the leading {@code >}
     * @param span Source span
     * @param home Source column where tail begins
     * @return Parsed result
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private static Parsed named(
        final String tail, final int from, final Span span, final int home
    ) {
        final int begin = Suffix.skipSpace(tail, from);
        if (begin >= tail.length()) {
            throw new ParseError(
                span.line(), home + from,
                "name suffix requires a name"
            );
        }
        int idx = begin;
        while (idx < tail.length() && !Suffix.endsName(tail.charAt(idx))) {
            idx = idx + 1;
        }
        final String name = tail.substring(begin, idx);
        if (name.codePoints().anyMatch(cp -> cp == 0x1F335)) {
            throw new ParseError(
                span.line(), home + begin,
                "cactus emoji is reserved for auto-names; not allowed in identifiers"
            );
        }
        boolean cnst = false;
        if (idx < tail.length() && tail.charAt(idx) == '!') {
            cnst = true;
            idx = idx + 1;
        }
        final int next = Suffix.skipSpace(tail, idx);
        final String signature;
        final int rest;
        if (next < tail.length() && tail.charAt(next) == '/') {
            if (cnst) {
                throw new ParseError(
                    span.line(), home + next,
                    "const and atom signature cannot be combined"
                );
            }
            signature = Suffix.signature(tail, next + 1, span, home);
            rest = next + 1 + signature.length();
        } else {
            signature = "";
            rest = idx;
        }
        Suffix.endsClean(tail, rest, span, home);
        return new Suffix.Parsed(Form.NAME, name, signature, cnst);
    }

    /**
     * Verify the rest of {@code tail} from {@code from} onward contains
     * only whitespace; otherwise raise a "trailing garbage" parse error.
     * @param tail Tail substring
     * @param from Index after the consumed suffix
     * @param span Source span
     * @param home Source column where tail begins
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private static void endsClean(
        final String tail, final int from, final Span span, final int home
    ) {
        int idx = from;
        while (idx < tail.length()
            && (tail.charAt(idx) == ' ' || tail.charAt(idx) == '\t')) {
            idx = idx + 1;
        }
        if (idx < tail.length()) {
            throw new ParseError(
                span.line(), home + idx,
                "trailing garbage after name suffix"
            );
        }
    }

    /**
     * Read the atom signature after the {@code /} marker.
     *
     * <p>The signature is a single {@code NAME} or a dotted path of
     * names, optionally rooted at {@code Q} (R-3.10.10). A leading
     * {@code Q.} is promoted to {@code Φ.} per R-3.10.11 / R-9.3. A
     * bare {@code Q} (root alone) is rejected.</p>
     *
     * @param tail Tail substring
     * @param after Index immediately after {@code /}
     * @param span Source span
     * @param home Source column where tail begins
     * @return Promoted signature
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private static String signature(
        final String tail, final int after, final Span span, final int home
    ) {
        int idx = after;
        while (idx < tail.length() && !Suffix.terminates(tail.charAt(idx))) {
            idx = idx + 1;
        }
        if (idx == after) {
            throw new ParseError(
                span.line(), home + after,
                "atom signature requires a name"
            );
        }
        final String raw = tail.substring(after, idx);
        if (raw.indexOf('?') >= 0) {
            throw new ParseError(
                span.line(), home + after,
                "optional marker ? is allowed only on a void attribute"
            );
        }
        if (raw.equals("Q")) {
            throw new ParseError(
                span.line(), home + after,
                "atom signature requires a name"
            );
        }
        return Suffix.typeAtom(raw, span, home + after);
    }

    /**
     * Skip exactly one space character.
     * @param tail Tail substring
     * @param from Current index
     * @return Index after the single space (or unchanged if at end)
     */
    private static int skipSpace(final String tail, final int from) {
        int idx = from;
        if (idx < tail.length() && tail.charAt(idx) == ' ') {
            idx = idx + 1;
        }
        return idx;
    }

    /**
     * Whether a character terminates a NAME / signature token. Mirrors
     * the {@code NAME} token rule of §2.3 plus the suffix-specific
     * separators ({@code !}, {@code /}).
     * @param glyph The character
     * @return True if the character ends the current token
     */
    private static boolean terminates(final char glyph) {
        return glyph == ' '
            || glyph == '\t'
            || glyph == '!'
            || glyph == '/';
    }

    /**
     * Whether a character terminates a name (stricter than
     * {@link #terminates}). NAME tokens forbid the separators
     * enumerated in {@link #NAME_BOUNDARIES}.
     * @param glyph The character
     * @return True if the character ends the NAME token
     */
    private static boolean endsName(final char glyph) {
        return Suffix.terminates(glyph)
            || Suffix.NAME_BOUNDARIES.indexOf(glyph) >= 0;
    }

    /**
     * Suffix form taxonomy.
     * @since 0.1
     */
    enum Form {

        /**
         * No suffix.
         */
        NONE,

        /**
         * Explicit name binding ({@code > name}).
         */
        NAME,

        /**
         * Auto-generated name ({@code >>}), optional handle (§3.10).
         */
        AUTO,

        /**
         * Truthy test attribute ({@code +> name}).
         */
        TEST,

        /**
         * Throwing test attribute ({@code -> name}) — the test is
         * expected to throw an exception.
         */
        THROWS
    }

    /**
     * Internal parse result.
     * @since 0.1
     */
    private static final class Parsed {

        /**
         * Form.
         */
        private final Form form;

        /**
         * Bound name.
         */
        private final String label;

        /**
         * Atom signature.
         */
        private final String sig;

        /**
         * Const marker.
         */
        private final boolean constant;

        /**
         * Ctor.
         * @param sform Form
         * @param slabel Label
         * @param ssig Signature
         * @param sconstant Const marker
         * @checkstyle ParameterNumberCheck (10 lines)
         */
        private Parsed(
            final Form sform, final String slabel, final String ssig, final boolean sconstant
        ) {
            this.form = sform;
            this.label = slabel;
            this.sig = ssig;
            this.constant = sconstant;
        }
    }
}
