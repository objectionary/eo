/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
package org.eolang.maven.rust_project;

import java.io.IOException;
import org.eolang.maven.footprint.Footprint;

/**
 * Class for creating and saving class with native method.
 * Created class then is used from {@link EOrust}.
 *
 * @since 0.30
 */
public final class Native {

    /**
     * The name of created java class.
     */
    private final String name;

    /**
     * Package of the class.
     */
    private final String pack;

    /**
     * Ctor.
     * @param name Name of the class.
     * @param pack Package of the class.
     */
    public Native(final String name, final String pack) {
        this.name = name;
        this.pack = pack;
    }

    /**
     * Save the class.
     * @param footprint Footprint to save.
     * @throws IOException If any issues with IO.
     */
    public void save(final Footprint footprint) throws IOException {
        footprint.save(
            this.name,
            "java",
            () -> String.join(
                System.lineSeparator(),
                String.format(
                    "package %s;",
                    this.pack
                ),
                String.format(
                    "public class %s {",
                    this.name
                ),
                String.format(
                    "    public static native int %s ();",
                    this.name
                ),
                "}"
            )
        );
    }
}
