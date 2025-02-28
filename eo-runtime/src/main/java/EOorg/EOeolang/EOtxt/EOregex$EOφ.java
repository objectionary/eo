/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOtxt; // NOPMD

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Regex.@ object.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "regex.@")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOregex$EOφ extends PhDefault implements Atom {
    @Override
    public Phi lambda() {
        final Phi regex = this.take(Attr.RHO);
        final String expression = new Dataized(regex.take("expression")).asString();
        if (!expression.startsWith("/")) {
            throw new ExFailure("Wrong regex syntax: \"/\" is missing");
        }
        final int last = expression.lastIndexOf('/');
        final StringBuilder builder = new StringBuilder();
        if (!expression.endsWith("/")) {
            builder.append("(?").append(expression.substring(last + 1)).append(')');
        }
        builder.append(expression, 1, last);
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final Phi pattern = regex.take("pattern");
        try {
            final ObjectOutputStream ous = new ObjectOutputStream(baos);
            final Pattern compiled = Pattern.compile(builder.toString());
            ous.writeObject(compiled);
            pattern.put(0, new Data.ToPhi(baos.toByteArray()));
            ous.close();
            return pattern;
        } catch (final PatternSyntaxException exception) {
            throw new ExFailure(
                "Regular expression syntax is invalid",
                exception
            );
        } catch (final IOException ex) {
            throw new IllegalArgumentException(ex);
        }
    }
}
