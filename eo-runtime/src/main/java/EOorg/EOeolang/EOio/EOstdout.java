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

import java.io.PrintStream;
import org.eolang.AtFree;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * Stdout.
 *
 * @since 0.1
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "stdout")
public final class EOstdout extends PhDefault implements Atom {
    /**
     * Default out print stream.
     */
    private static final PrintStream OUT = System.out;

    /**
     * Stream to print out.
     */
    private final PrintStream out;

    /**
     * Default ctor.
     * @param sigma Sigma
     */
    public EOstdout(final Phi sigma) {
        this(sigma, EOstdout.OUT);
    }

    /**
     * Ctor for the tests.
     * @param sigma Sigma
     * @param out Stream to print
     */
    EOstdout(final Phi sigma, final PrintStream out) {
        super(sigma);
        this.out = out;
        this.add("text", new AtFree());
    }

    @Override
    public Phi lambda() {
        this.out.print(
            new Param(this, "text").strong(String.class)
        );
        return new Data.ToPhi(true);
    }
}
