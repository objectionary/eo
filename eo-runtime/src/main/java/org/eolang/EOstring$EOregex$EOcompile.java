/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Regex.compile object.
 *
 * <p>A pattern that does not compile is reported with the construct the
 * engine choked on and the offset it sits at, so that six different
 * mistakes no longer read the same (#7986). The flag group is compiled in
 * front of the pattern, so its length is taken off the index the engine
 * reports, leaving an offset into the pattern the caller wrote; an error
 * inside the flag group itself lands before that start and is reported
 * without an offset.</p>
 *
 * <p>What stands after the closing slash is a flag section, and it is
 * checked against the same alphabet the EO half of this object reads with
 * ({@code [dimsux]}) before it is spliced between {@code (?} and {@code )}.
 * Without that check a literal such as {@code /b/i)|(?:a} closed the flag
 * group and injected its own alternation into the pattern, so the regex that
 * ran was not the one that was written, and nothing reported it.</p>
 *
 * @since 0.39.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "string.regex.compile")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOstring$EOregex$EOcompile extends PhDefault implements Atom {

    /**
     * Name of the error-branch void that holds the caller's compile fallback.
     */
    private static final String FALLBACK = "cant-compile";

    /**
     * Ctor.
     */
    public EOstring$EOregex$EOcompile() {
        super(new Attrs(
            new Attr(Phi.RHO, new AtRho()),
            new Attr(
                EOstring$EOregex$EOcompile.FALLBACK,
                new AtVoid(EOstring$EOregex$EOcompile.FALLBACK)
            )
        ));
    }

    @Override
    public Phi lambda() {
        final String expression = new Dataized(this.take(Phi.RHO).take(Phi.RHO)).asString();
        final int last = expression.lastIndexOf('/');
        final Phi result;
        if (expression.startsWith("/")) {
            if (last == 0) {
                result = this.fallback("regex is missing the closing slash");
            } else {
                result = this.compile(expression, last);
            }
        } else {
            result = this.fallback("regex is missing the opening slash");
        }
        return result;
    }

    private Phi fallback(final String message) {
        final Phi cant = this.take(EOstring$EOregex$EOcompile.FALLBACK);
        cant.put(0, new Data.ToPhi(message));
        return cant;
    }

    private Phi compile(final String expression, final int last) {
        final String modifiers = expression.substring(last + 1);
        final Phi result;
        if (modifiers.chars().anyMatch(chr -> "dimsux".indexOf(chr) < 0)) {
            result = this.fallback(
                String.format(
                    "regex flags '%s' must be a sequence of 'd', 'i', 'm', 's', 'u' and 'x'",
                    modifiers
                )
            );
        } else {
            result = this.pattern(expression, last, modifiers);
        }
        return result;
    }

    private Phi pattern(final String expression, final int last, final String modifiers) {
        final StringBuilder builder = new StringBuilder();
        if (!modifiers.isEmpty()) {
            builder.append("(?").append(modifiers).append(')');
        }
        final int flags = builder.length();
        builder.append(expression, 1, last);
        Phi outcome;
        try {
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            final ObjectOutputStream ous = new ObjectOutputStream(baos);
            ous.writeObject(Pattern.compile(builder.toString()));
            ous.close();
            outcome = this.take(Phi.RHO).take("pattern");
            outcome.put(0, new Data.ToPhi(baos.toByteArray()));
        } catch (final PatternSyntaxException ex) {
            final int offset = ex.getIndex() - flags;
            final String reason;
            if (ex.getIndex() < 0 || offset < 0) {
                reason = String.format("regex syntax is invalid: %s", ex.getDescription());
            } else {
                reason = String.format(
                    "regex syntax is invalid: %s at offset %d of the pattern",
                    ex.getDescription(), offset
                );
            }
            outcome = this.fallback(reason);
        } catch (final IOException ex) {
            throw new ExFailure("cannot serialize the compiled regex pattern", ex);
        }
        return outcome;
    }
}
