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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.eolang.compiler.syntax.Tree;

/**
 * Program.
 *
 * @author Yegor Bugayenko (yegor256@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class Program {

    /**
     * Text to parse.
     */
    private final String text;

    /**
     * Ctor.
     * @param input Input text
     */
    public Program(final String input) {
        this.text = input;
    }

    /**
     * Compile it to Java and save to the directory.
     * @param dir Directory to save to
     * @throws IOException If fails
     */
    public void save(final Path dir) throws IOException {
        final ProgramLexer lexer = new ProgramLexer(
            new ANTLRInputStream(
                new ByteArrayInputStream(
                    this.text.getBytes(Charset.defaultCharset())
                )
            )
        );
        final TokenStream tokens = new CommonTokenStream(lexer);
        final ProgramParser parser = new ProgramParser(tokens);
        final Tree tree = parser.program().ret;
        tree.java().entrySet().stream().forEach(
            entry -> Program.save(
                dir.resolve(entry.getKey()), entry.getValue()
            )
        );
    }

    /**
     * Save content.
     * @param file File to save to
     * @param content Java content
     */
    private static void save(final Path file, final String content) {
        try {
            Files.write(file, content.getBytes());
        } catch (final IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

}
