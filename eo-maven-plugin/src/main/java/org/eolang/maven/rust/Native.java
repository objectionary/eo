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
package org.eolang.maven.rust;

/**
 * Class for creating and saving class with native method.
 * Created class then is used from {@link EOrust}.
 *
 * @since 0.30
 */
public final class Native extends Savable {

    /**
     * Package of the java file.
     */
    private final String pack;

    /**
     * Ctor.
     * @param name Name of the class.
     * @param pack Package of the class.
     */
    public Native(final String name, final String pack) {
        super(
            name,
            "java"
        );
        this.pack = pack;
    }

    @Override
    String content() {
        return String.join(
            System.lineSeparator(),
            String.format(
                "package %s;",
                this.pack
            ),
            "import org.eolang.Universe;",
            String.format(
                "public class %s {",
                this.name
            ),
            String.format(
                "    public static native byte[] %s",
                this.name
            ),
            "        (final Universe universe);",
            "}"
        );
    }
}
