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
package EOorg.EOeolang.EOfs; // NOPMD

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * File.open.file-stream.write.written-bytes.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "file.open.file-stream.write.written-bytes")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOfile$EOopen$EOfile_stream$EOwrite$EOwritten_bytes
    extends PhDefault
    implements Atom {

    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOfile$EOopen$EOfile_stream$EOwrite$EOwritten_bytes() {
        this.add("buffer", new AtVoid("buffer"));
    }

    @Override
    public Phi lambda() {
        final Path path = Paths.get(
            new Dataized(
                this.take(Attr.RHO)
                    .take(Attr.RHO)
                    .take(Attr.RHO)
                    .take(Attr.RHO)
                    .take("path")
            ).asString()
        );
        try {
            Files.INSTANCE.write(
                path.toString(),
                new Dataized(this.take("buffer")).take()
            );
        } catch (final IOException ex) {
            throw new IllegalArgumentException(
                String.format("Can't write to %s", path),
                ex
            );
        }
        return new Data.ToPhi(true);
    }
}
