/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
 * Regex.compiled.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "regex.compiled")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOregex$EOcompiled extends PhDefault implements Atom {
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
