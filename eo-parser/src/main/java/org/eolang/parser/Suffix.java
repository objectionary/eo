/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Set;
import java.util.regex.Pattern;

/**
 * A parsed name suffix — §3.10 of the spec.
 *
 * <p>Recognises the four base forms (mutually exclusive on any line)
 * plus the {@code !} const modifier and the {@code /sig} atom signature:</p>
 *
 * <ul>
 * <li>{@code > name} — explicit name binding.</li>
 * <li>{@code >>} — auto-generated name, optional handle (§3.10).</li>
 * <li>{@code +> name} — truthy test attribute.</li>
 * <li>{@code -> name} — throwing test attribute (expected to throw).</li>
 * <li>(empty) — no suffix.</li>
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
 * <li>R-3.10.2 — {@code >>} cannot carry {@code /sig}.</li>
 * <li>R-3.10.3 — {@code > name!} cannot combine with {@code /sig}.</li>
 * <li>R-3.10.10 — {@code /sig} must be a non-empty dotted name
 * (optionally rooted at {@code Q}) or a generic type variable
 * {@code A}–{@code F}; a bare {@code /}, {@code /Q} alone, or a
 * {@code ?} optional marker (void-only) is rejected.</li>
 * <li>R-6.3.5 — a {@code +>} test name must be a {@code NAME} token,
 * not {@code @} (PHI).</li>
 * </ul>
 *
 * @since 0.1
 */
final class Suffix {

    /**
     * Scope tokens, which name a place rather than an object: the
     * {@code φ} decoratee, the {@code ρ} parent and {@code ξ} itself.
     * A {@code >>} handle is a name a later reference can be written
     * with, so none of these can serve as one, the same way
     * {@link #test(String, int, Span, int, Form)} already refuses
     * {@code @} for a test attribute.
     */
    private static final Set<String> SCOPES = Set.of("@", "^", "$");

    /**
     * A generic type variable — one uppercase letter of {@code A}–{@code F}
     * (R-3.10.10).
     */
    private static final Pattern VARIABLE = Pattern.compile("[A-F]");

    /**
     * One {@code NAME} token per §2.3 — a lowercase letter, then anything
     * but a token boundary.
     */
    private static final Pattern NAME = Pattern.compile(
        "[a-z][^ \\t,.|':;!?\\[\\]{}()]*"
    );

    /**
     * Suffix form.
     */
    private final Form form;

    /**
     * Bound name for {@code NAME} / {@code TEST} / {@code THROWS} forms;
     * the file-local handle for a {@code >> name} {@code AUTO} suffix;
     * empty otherwise.
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
     */
    Suffix(final String tail, final Span span, final int home) {
        this(Suffix.parse(tail, span, home));
    }

    /**
     * Ctor — copies the fields of an already parsed suffix, since a
     * constructor can't hand back an instance the parser has already
     * built.
     * @param result Parsed suffix
     */
    private Suffix(final Suffix result) {
        this(result.form, result.label, result.sig, result.constant);
    }

    /**
     * Primary ctor.
     * @param sform Form
     * @param slabel Bound name
     * @param ssig Atom signature
     * @param sconstant Const marker
     */
    private Suffix(
        final Form sform, final String slabel, final String ssig, final boolean sconstant
    ) {
        this.form = sform;
        this.label = slabel;
        this.sig = ssig;
        this.constant = sconstant;
    }

    /**
     * The suffix form — one of {@code NONE}, {@code NAME}, {@code AUTO},
     * {@code TEST}, {@code THROWS}.
     * @return Form
     */
    Form form() {
        return this.form;
    }

    /**
     * Bound name. Empty for {@code NONE}; for {@code AUTO} it is the
     * file-local handle of a {@code >> name} suffix, empty only for a
     * bare {@code >>}.
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
     * this suffix, applying R-9.3 source-token mapping: {@code @} becomes
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
     * Reject this suffix's atom signature if it was parsed on a line that
     * is not a formation — §3.10.10 of the spec. Only a {@link LnFormation}
     * ever reads the signature back out to emit the atom marker; every
     * other line shape that can carry a name suffix is no more a
     * formation than a pipe is, so a {@code /sig} written on one of them
     * is the same user mistake, worth the same message regardless of
     * which line shape it was written on (#6230).
     * @param span The line's span (used for error position)
     */
    void rejectAtomOutsideFormation(final Span span) {
        if (this.atom()) {
            throw new ParseError(
                span.line(), span.indent(),
                "only a formation can declare an atom signature"
            );
        }
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
     * <p>A concrete forma is a {@code NAME ('.' NAME)*} path, so a
     * leading dot, a trailing dot, an empty segment, or a segment that
     * is not a {@code NAME} token (§2.3) is rejected here, for the
     * signature and the annotation alike. A scope token ({@code @},
     * {@code ^}, {@code $}) and anything else that does not open with a
     * lowercase letter therefore names no type.</p>
     *
     * @param raw Raw token, without a trailing {@code ?}
     * @param span Source span
     * @param pos Source column of the token (for errors)
     * @return Emitted token — variable verbatim, forma promoted
     */
    static String typeAtom(final String raw, final Span span, final int pos) {
        Suffix.checkGlyphs(raw, span, pos);
        final char first = raw.charAt(0);
        if (first >= 'A' && first <= 'Z'
            && !Suffix.VARIABLE.matcher(raw).matches() && !raw.startsWith("Q.")) {
            throw new ParseError(
                span.line(), pos,
                "type variable must be one of A-F"
            );
        }
        if (raw.startsWith(".") || raw.endsWith(".") || raw.contains("..")) {
            throw new ParseError(
                span.line(), pos,
                "type must be a dotted name with no leading, trailing, or empty segment"
            );
        }
        if (!Suffix.VARIABLE.matcher(raw).matches()) {
            Suffix.checkPath(raw, span, pos);
        }
        final String promoted;
        if (raw.startsWith("Q.")) {
            promoted = "Φ".concat(raw.substring(1));
        } else {
            promoted = raw;
        }
        return promoted;
    }

    private static String phi(final String raw) {
        final String mapped;
        if ("@".equals(raw)) {
            mapped = "φ";
        } else {
            mapped = raw;
        }
        return mapped;
    }

    private static Suffix parse(final String tail, final Span span, final int home) {
        final int idx = Suffix.start(tail);
        final Suffix result;
        if (idx >= tail.length()) {
            result = new Suffix(Form.NONE, "", "", false);
        } else if (tail.startsWith("+>", idx)) {
            result = Suffix.test(tail, idx + 2, span, home, Form.TEST);
        } else if (tail.startsWith("->", idx)) {
            result = Suffix.test(tail, idx + 2, span, home, Form.THROWS);
        } else if (tail.startsWith(">>", idx)) {
            result = Suffix.auto(tail, idx + 2, span, home);
        } else if (tail.charAt(idx) == '>') {
            result = Suffix.named(tail, idx + 1, span, home);
        } else if (tail.charAt(idx) == '!') {
            Suffix.endsClean(tail, idx + 1, span, home);
            result = new Suffix(Form.NONE, "", "", true);
        } else {
            throw new ParseError(
                span.line(), home + idx,
                "unexpected content after name suffix"
            );
        }
        return result;
    }

    private static int start(final String tail) {
        int idx = 0;
        while (idx < tail.length() && tail.charAt(idx) == ' ') {
            idx = idx + 1;
        }
        return idx;
    }

    private static Suffix test(
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
        idx = Suffix.skipName(tail, idx);
        if (start == idx) {
            throw new ParseError(
                span.line(), home + start,
                "test attribute requires a name"
            );
        }
        final String name = tail.substring(start, idx);
        Suffix.checkGlyphs(name, span, home + start);
        Suffix.checkLowercaseStart(name, span, home, start);
        Suffix.endsClean(tail, idx, span, home);
        return new Suffix(form, name, "", false);
    }

    private static void checkPath(final String raw, final Span span, final int pos) {
        int from = 0;
        if (raw.startsWith("Q.")) {
            from = 2;
        }
        while (from < raw.length()) {
            int end = raw.indexOf('.', from);
            if (end < 0) {
                end = raw.length();
            }
            if (!Suffix.NAME.matcher(raw.substring(from, end)).matches()) {
                throw new ParseError(
                    span.line(), pos,
                    "type must be a dotted path of NAME tokens"
                );
            }
            from = end + 1;
        }
    }

    private static void checkGlyphs(final String name, final Span span, final int pos) {
        if (name.codePoints().anyMatch(cp -> cp == 0x1F335)) {
            throw new ParseError(
                span.line(), pos,
                "cactus emoji is reserved for auto-names; not allowed in identifiers"
            );
        }
        final int control = new Scrubbed(name).found();
        if (control >= 0) {
            throw new ParseError(
                span.line(), pos + control,
                "control character is not allowed in an identifier"
            );
        }
    }

    private static void checkLowercaseStart(
        final String name, final Span span, final int home, final int pos
    ) {
        if (!name.isEmpty() && !"@".equals(name)
            && (name.charAt(0) < 'a' || name.charAt(0) > 'z')) {
            throw new ParseError(
                span.line(), home + pos,
                "name must start with a lowercase letter"
            );
        }
    }

    private static void checkNamePresent(
        final String tail, final int begin, final int idx, final Span span, final int home
    ) {
        if (begin == idx && tail.charAt(begin) != ' ' && tail.charAt(begin) != '\t') {
            throw new ParseError(
                span.line(), home + begin,
                "name suffix requires a name"
            );
        }
    }

    private static Suffix auto(
        final String tail, final int after, final Span span, final int home
    ) {
        int idx = after;
        boolean cnst = false;
        if (tail.startsWith("!", idx)) {
            cnst = true;
            idx = idx + 1;
        }
        final int begin = Suffix.skipSpace(tail, idx);
        int rest = Suffix.skipName(tail, begin);
        final String handle = tail.substring(begin, rest);
        if (Suffix.SCOPES.contains(handle)) {
            throw new ParseError(
                span.line(), home + begin,
                String.format(
                    "file-local handle must be an identifier, not %s", handle
                )
            );
        }
        Suffix.checkGlyphs(handle, span, home + begin);
        Suffix.checkLowercaseStart(handle, span, home, begin);
        if (!cnst && tail.startsWith("!", rest)) {
            cnst = true;
            rest = rest + 1;
        }
        final int trailing = Suffix.skipSpace(tail, rest);
        if (tail.startsWith("/", trailing)) {
            throw new ParseError(
                span.line(), home + trailing,
                "auto-named atom is forbidden"
            );
        }
        Suffix.endsClean(tail, trailing, span, home);
        return new Suffix(Form.AUTO, handle, "", cnst);
    }

    private static Suffix named(
        final String tail, final int from, final Span span, final int home
    ) {
        if (Suffix.blank(tail, from)) {
            throw new ParseError(
                span.line(), home + from,
                "name suffix requires a name"
            );
        }
        final int begin = Suffix.skipSpace(tail, from);
        int idx = Suffix.skipName(tail, begin);
        Suffix.checkNamePresent(tail, begin, idx, span, home);
        final String name = tail.substring(begin, idx);
        Suffix.checkGlyphs(name, span, home + begin);
        Suffix.checkLowercaseStart(name, span, home, begin);
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
        return new Suffix(Form.NAME, name, signature, cnst);
    }

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
                "unexpected content after name suffix"
            );
        }
    }

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
        if ("Q".equals(raw)) {
            throw new ParseError(
                span.line(), home + after,
                "atom signature requires a name"
            );
        }
        return Suffix.typeAtom(raw, span, home + after);
    }

    private static boolean blank(final String tail, final int from) {
        int idx = from;
        while (idx < tail.length()
            && (tail.charAt(idx) == ' ' || tail.charAt(idx) == '\t')) {
            idx = idx + 1;
        }
        return idx >= tail.length();
    }

    private static int skipSpace(final String tail, final int from) {
        int idx = from;
        while (idx < tail.length() && tail.charAt(idx) == ' ') {
            idx = idx + 1;
        }
        return idx;
    }

    private static int skipName(final String tail, final int from) {
        int idx = from;
        while (idx < tail.length() && !Suffix.endsName(tail.charAt(idx))) {
            idx = idx + 1;
        }
        return idx;
    }

    private static boolean terminates(final char glyph) {
        return glyph == ' '
            || glyph == '\t'
            || glyph == '!'
            || glyph == '/';
    }

    private static boolean endsName(final char glyph) {
        return Suffix.terminates(glyph)
            || ",.|':;?[]{}()".indexOf(glyph) >= 0;
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
}
