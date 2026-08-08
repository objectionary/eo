/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Regex.compile.assembled object.
 *
 * <p>Everything that surrounds the pattern in the Perl-format expression is
 * taken apart by {@code compile} in EO: the opening and the closing slash are
 * validated there, the flag letters are cut off the tail, and what is left is
 * the {@code search} string this object receives. All that stays here is the
 * one step the JVM has to do, compiling that string and serializing the
 * result, plus the syntax error that only compiling can discover.</p>
 *
 * @since 0.39.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "regex.compile.assembled")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOregex$EOcompile$EOassembled extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOregex$EOcompile$EOassembled() {
        // nothing
    }

    @Override
    public Phi lambda() {
        final Phi compile = this.take(Phi.RHO);
        Phi result;
        try {
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            final ObjectOutputStream ous = new ObjectOutputStream(baos);
            ous.writeObject(
                Pattern.compile(new Dataized(compile.take("search")).asString())
            );
            ous.close();
            result = compile.take(Phi.RHO).take("pattern");
            result.put(0, new Data.ToPhi(baos.toByteArray()));
        } catch (final PatternSyntaxException ex) {
            result = compile.take("cant-compile");
            result.put(0, new Data.ToPhi("regex syntax is invalid"));
        } catch (final IOException ex) {
            throw new ExFailure("cannot serialize the compiled regex pattern", ex);
        }
        return result;
    }
}
