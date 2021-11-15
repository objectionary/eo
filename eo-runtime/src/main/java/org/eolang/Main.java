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

package org.eolang;

import EOorg.EOeolang.EOstring;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/**
 * Bridge between Java CLI and EO.
 *
 * @since 0.1
 */
public final class Main {

    /**
     * Logger.
     */
    private static final Logger LOGGER = Logger.getLogger(Main.class.getName());

    /**
     * The method caled by JVM when the program starts.
     *
     * @param args Command line args
     * @throws Exception If fails
     */
    public static void main(final String... args) throws Exception {
        final List<String> opts = new ArrayList<>(args.length);
        opts.addAll(Arrays.asList(args));
        while (!opts.isEmpty()) {
            final String opt = opts.get(0);
            if (Main.parse(opt)) {
                return;
            }
            if (!opt.startsWith("--")) {
                break;
            }
            opts.remove(0);
        }
        Main.LOGGER.log(Level.FINE, String.format("EOLANG Runtime %s", Main.version()));
        if (opts.isEmpty()) {
            throw new IllegalStateException(
                "A name of EO object is expected as a command line argument"
            );
        }
        Main.run(opts);
    }

    /**
     * Process one option.
     * @param opt The option
     * @return TRUE if it's time to exit
     * @throws IOException If fails
     */
    private static boolean parse(final String opt) throws IOException {
        boolean exit = false;
        if ("--verbose".equals(opt)) {
            final Logger log = Logger.getLogger("org.eolang");
            log.setLevel(Level.FINE);
            final Handler handler = new ConsoleHandler();
            handler.setFormatter(
                new Formatter() {
                    @Override
                    public String format(final LogRecord rec) {
                        return String.format("%s%n", rec.getMessage());
                    }
                }
            );
            for (final Handler hnd : log.getHandlers()) {
                hnd.setLevel(Level.FINE);
            }
            log.addHandler(handler);
            log.setUseParentHandlers(false);
        } else if ("--version".equals(opt)) {
            Main.LOGGER.info(Main.version());
            exit = true;
        } else if ("--help".equals(opt)) {
            Main.LOGGER.info(
                String.join(
                    "\n",
                    "Usage: java eo-runtime.jar [option...] class [argument...]",
                    "  class: Name of EO class, e.g. \"org.eolang.io.stdio\"",
                    "  argument: Value that will be wrapped as strings and passed to your EO object",
                    "  options:",
                    "    --help     Print this documentation and exit",
                    "    --version  Print the version of this JAR and exit",
                    "    --verbose  Print all intermediate dataization results"
                )
            );
            exit = true;
        }
        return exit;
    }

    /**
     * Run this opts.
     * @param opts The opts left
     * @throws Exception If fails
     */
    private static void run(final List<String> opts) throws Exception {
        final String path = Arrays.stream(opts.get(0).split("\\."))
            .map(p -> String.format("EO%s", p))
            .collect(Collectors.joining("."));
        final Phi app = Phi.class.cast(
            Class.forName(path).getConstructor(Phi.class)
                .newInstance(Phi.Φ)
        );
        for (int idx = 1; idx < opts.size(); ++idx) {
            final Phi phi = new EOstring(Phi.Φ);
            final String arg = opts.get(idx);
            phi.attr("Δ").put(new Data.Value<>(arg));
            app.attr(0).put(phi);
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
    private static String version() throws IOException {
        try (BufferedReader input =
            new BufferedReader(
                new InputStreamReader(
                    Objects.requireNonNull(Main.class.getResourceAsStream("version.txt")),
                    StandardCharsets.UTF_8
                )
            )
        ) {
            return input.lines().findFirst().get();
        }
    }

}
