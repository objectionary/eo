/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
 */
package EOorg.EOeolang.EOio;

import java.util.NoSuchElementException;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;

/**
 * Standard Input. Consumes only one line.
 *
 * @since 0.23
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
public final class EOstdin$EOnext_line extends PhDefault implements Atom {
    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOstdin$EOnext_line(final Phi sigma) {
        super(sigma);
    }

    @Override
    public Phi lambda() {
        try {
            final Input input = Input.getInstance();
            final String line = input.getLine();
            return new Data.ToPhi(line);
        } catch (final NoSuchElementException exception) {
            throw new ExFailure(
                "There is no line in the standard input stream to consume"
            );
        }
    }
}
