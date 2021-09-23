/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

package org.eolang.phi;

import EOorg.EOeolang.EOstring;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Bridge between Java CLI and EO.
 *
 * @since 0.1
 */
public final class Main {

    private final PrintStream stdout;

    /**
     * Ctor.
     * @param out The output
     */
    public Main(final PrintStream out) {
        this.stdout = out;
    }

    /**
     * The method caled by JVM when the program starts.
     * @param args Command line args
     * @throws Exception If fails
     */
    public static void main(final String... args) throws Exception {
        new Main(System.out).exec(args);
    }

    /**
     * The same method, but not static.
     * @param args Command line args
     * @throws Exception If fails
     */
    public void exec(final String... args) throws Exception {
        if (args.length == 0 || "--version".equals(args[0])) {
            this.version();
            return;
        }
        final String path = Arrays.stream(args[0].split("\\."))
            .map(p -> String.format("EO%s", p))
            .collect(Collectors.joining("."));
        final Phi app = Phi.class.cast(
            Class.forName(path).getConstructor().newInstance()
        );
        for (int idx = 1; idx < args.length; ++idx) {
            final Phi phi = new EOstring();
            final String arg = args[idx];
            phi.attr("Δ").put(new Data.Value<>(arg));
            app.attr("args").put(phi);
        }
        if (!new Dataized(app).take(Boolean.class)) {
            throw new IllegalStateException(
                "Runtime dataization failure"
            );
        }
    }

    /**
     * Read the version from resources and prints it.
     * @throws IOException If fails
     */
    private void version() throws IOException {
        try (BufferedReader input =
            new BufferedReader(
                new InputStreamReader(
                    Main.class.getResourceAsStream("version.txt"),
                    StandardCharsets.UTF_8)
                )
            ) {
            this.stdout.printf(
                "EOLANG Runtime v.%s",
                input.lines().findFirst().get()
            );
        }
    }

}
