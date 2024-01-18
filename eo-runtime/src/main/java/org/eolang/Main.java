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

package org.eolang;

import EOorg.EOeolang.EOtuple;
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

/**
 * Bridge between Java CLI and EO.
 *
 * This class has a single public static method {@code main()}, which
 * is supposed to be called by java runtime from command line. The best
 * example of this class usage is in the "sandbox/canonical" directory.
 *
 * @since 0.1
 */
@Versionized
public final class Main {

    /**
     * Logger.
     */
    private static final Logger LOGGER = Logger.getLogger(Main.class.getName());

    /**
     * EO app-wide logger.
     */
    private static final Logger EOLOG = Logger.getLogger("org.eolang");

    /**
     * Not for instantiation.
     */
    private Main() {
    }

    /**
     * The method called by JVM when the program starts.
     *
     * @param args Command line args
     * @throws Exception If fails
     */
    public static void main(final String... args) throws Exception {
        Main.setup();
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
        try {
            Main.run(opts);
        } catch (final ExAbstract ex) {
            Main.print(ex);
            System.exit(1);
        }
    }

    /**
     * Print exception line.
     *
     * @param thr The cause
     */
    private static void print(final Throwable thr) {
        Main.LOGGER.log(Level.SEVERE, thr.getMessage());
        final Throwable cause = thr.getCause();
        if (cause != null) {
            Main.print(cause);
        }
    }

    /**
     * Setup logs.
     */
    private static void setup() {
        final Handler handler = new ConsoleHandler();
        handler.setFormatter(
            new Formatter() {
                @Override
                public String format(final LogRecord rec) {
                    return String.format("%s%n", rec.getMessage());
                }
            }
        );
        Main.EOLOG.addHandler(handler);
        Main.EOLOG.setUseParentHandlers(false);
    }

    /**
     * Process one option.
     * @param opt The option
     * @return TRUE if it's time to exit
     * @throws IOException If fails
     */
    private static boolean parse(final String opt) throws IOException {
        if ("--verbose".equals(opt)) {
            Main.EOLOG.setLevel(Level.FINE);
            for (final Handler hnd : Main.EOLOG.getHandlers()) {
                hnd.setLevel(Level.FINE);
            }
        }
        boolean exit = false;
        if ("--version".equals(opt)) {
            Main.LOGGER.info(Main.version());
            exit = true;
        }
        if ("--help".equals(opt)) {
            Main.LOGGER.info(
                String.join(
                    "\n",
                    "Usage: java -cp target/classes org.eolang.Main [option...] class [argument...]",
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
        final String path = new JavaPath(opts.get(0)).toString();
        final Phi app;
        try {
            Main.LOGGER.fine(String.format("Loading class %s...", path));
            app = Phi.class.cast(
                Class.forName(path).getConstructor(Phi.class)
                .newInstance(Phi.Φ)
            );
        } catch (final ClassNotFoundException ex) {
            throw new ExUnset(
                String.format("Can not find '%s' object", opts.get(0))
            );
        }
        if (opts.size() > 1) {
            Phi args = Phi.Φ.attr("org").get()
                .attr("eolang").get()
                .attr("tuple").get()
                .attr("empty").get();
            for (int idx = 1; idx < opts.size(); ++idx) {
                final Phi tuple = new EOtuple(Phi.Φ);
                tuple.attr(0).put(args);
                tuple.attr(1).put(new Data.ToPhi(opts.get(idx)));
                args = tuple;
            }
            app.attr(0).put(args);
        }
        Main.LOGGER.info(
            String.format(
                "%n---%n%s",
                new VerboseBytesAsString(new Dataized(app).take()).get()
            )
        );
    }

    /**
     * Read the version from resources and return it.
     * @return Version string
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
            return input.lines().findFirst().orElse("N/A");
        }
    }

}
