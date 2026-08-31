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
        final StringBuilder builder = new StringBuilder();
        if (!expression.endsWith("/")) {
            builder.append("(?").append(expression.substring(last + 1)).append(')');
        }
        final int flags = builder.length();
        builder.append(expression, 1, last);
        Phi result;
        try {
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            final ObjectOutputStream ous = new ObjectOutputStream(baos);
            ous.writeObject(Pattern.compile(builder.toString()));
            ous.close();
            result = this.take(Phi.RHO).take("pattern");
            result.put(0, new Data.ToPhi(baos.toByteArray()));
        } catch (final PatternSyntaxException ex) {
            result = this.fallback(this.explained(ex, flags));
        } catch (final IOException ex) {
            throw new ExFailure("cannot serialize the compiled regex pattern", ex);
        }
        return result;
    }

    /**
     * The message a failed compilation is reported with.
     *
     * <p>The engine walks the pattern with an index and names the construct
     * it choked on, so both go into the message: one message per mistake
     * instead of one for all of them (#7986). The index is counted from the
     * start of what was compiled, which is the flag group first when the
     * pattern carries flags, so the group is taken off to leave an offset
     * into the pattern the caller wrote. An error inside the flag group
     * itself lands before that start and is reported without an offset.</p>
     *
     * @param ex The failure the engine raised
     * @param flags Length of the flag group put in front of the pattern
     * @return The message
     */
    private String explained(final PatternSyntaxException ex, final int flags) {
        final int offset = ex.getIndex() - flags;
        final String result;
        if (ex.getIndex() < 0 || offset < 0) {
            result = String.format("regex syntax is invalid: %s", ex.getDescription());
        } else {
            result = String.format(
                "regex syntax is invalid: %s at offset %d of the pattern",
                ex.getDescription(), offset
            );
        }
        return result;
    }
}
