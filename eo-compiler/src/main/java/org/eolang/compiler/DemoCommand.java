/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 eolang.org
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
package org.eolang.compiler;

import java.io.IOException;

/**
 * Class comment.
 *
 * @author John Page (johnpagedev@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class DemoCommand implements EocCommand {
    /**
     * Attribute comment.
     */
    private final EocCommandArgument filename;

    /**
     * Method comment.
     *
     * @param filename F.
     */
    public DemoCommand(final EocCommandArgument filename) {
        this.filename = filename;
    }

    /**
     * Method comment.
     *
     * @return Something.
     * @throws IOException If.
     */
    public String output() throws IOException {
        final String output;
        final String path = "org\\eolang\\compiler\\eo\\";
        if (this.filename.isEmpty()) {
            output = String.format(
                "%s %s%s",
                "Use --demo <filename> to see one",
                "of the following files compiled:\n",
                new EoResourceFiles(path).formattedNames()
            );
        } else {
            output = String.join(
                "\n",
                new String[]{
                    new EoResourceFiles(path)
                        .eolang(this.filename.string()),
                    new EoResourceFiles(path)
                        .java(this.filename.string()),
                });
        }
        return output;
    }
}
