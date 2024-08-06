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

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * Console.read.read-bytes.
 *
 * @since 0.39
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "console.read.read-bytes")
public final class EOconsole$EOread$EOread_bytes extends PhDefault implements Atom {
    /**
     * Input stream to read bytes from.
     */
    private final InputStream input;

    /**
     * Ctor.
     */
    public EOconsole$EOread$EOread_bytes() {
        this(System.in);
    }

    /**
     * Ctor for the tests.
     * @param input Stream to read from
     */
    EOconsole$EOread$EOread_bytes(final InputStream input) {
        this.input = input;
        this.add("size", new AtVoid("size"));
    }

    @Override
    public Phi lambda() throws IOException {
        final int size = new Dataized(this.take("size")).asNumber().intValue();
        final byte[] read = new byte[size];
        int character;
        int processed = 0;
        while (processed < size && (character = this.input.read()) != -1) {
            read[processed] = (byte) character;
            ++processed;
        }
        return new Data.ToPhi(Arrays.copyOf(read, processed));
    }
}
