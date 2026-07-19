/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Attrs;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Regex.compile object.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "regex.compile")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOregex$EOcompile extends PhDefault implements Atom {

    /**
     * Name of the error-branch void that holds the caller's compile fallback.
     */
    private static final String FALLBACK = "cant-compile";

    /**
     * Ctor.
     */
    public EOregex$EOcompile() {
        super(new Attrs(
            new Attr(
                EOregex$EOcompile.FALLBACK,
                new AtVoid(EOregex$EOcompile.FALLBACK)
            )
        ));
    }

    @Override
    public Phi lambda() {
        final String expression = new Dataized(this.take(Phi.RHO).take("expression")).asString();
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

    /**
     * The caller-supplied {@code cant-compile} recovery for an expression that
     * cannot become a pattern, or the bottom object (⊥) when none was bound.
     * @param message Why the expression could not be compiled
     * @return The fallback object carrying the message
     */
    private Phi fallback(final String message) {
        final Phi cant = this.take(EOregex$EOcompile.FALLBACK);
        cant.put(0, new Data.ToPhi(message));
        return cant;
    }

    /**
     * Compile the expression into a serialized pattern, or fall back when its
     * body is invalid. An invalid body is only discoverable by compiling, so
     * the {@link PatternSyntaxException} is caught at that boundary and routed
     * to the fallback; a serialization {@link IOException} on an in-memory
     * stream is unpredictable and aborts as a system failure.
     * @param expression The Perl-format expression, slashes included
     * @param last Index of the closing slash in the expression
     * @return The compiled pattern, or the {@code cant-compile} fallback
     */
    private Phi compile(final String expression, final int last) {
        final StringBuilder builder = new StringBuilder();
        if (!expression.endsWith("/")) {
            builder.append("(?").append(expression.substring(last + 1)).append(')');
        }
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
            result = this.fallback("regex syntax is invalid");
        } catch (final IOException ex) {
            throw new ExFailure("cannot serialize the compiled regex pattern", ex);
        }
        return result;
    }
}
