/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
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
 * <p>This class has a single public static method {@code main()}, which
 * is supposed to be called by java runtime from command line. The best
 * example of this class usage is in the "sandbox/canonical" directory.</p>
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.MoreThanOneLogger")
public final class Main {

    /**
     * Verbose option.
     */
    static final String VERBOSE = "--verbose";

    /**
     * Version option.
     */
    private static final String VERSION = "--version";

    /**
     * Help option.
     */
    private static final String HELP = "--help";

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
        final List<String> opts = Arrays.stream(args)
            .filter(Main::isOption)
            .collect(Collectors.toList());
        for (final String opt : opts) {
            if (Main.parse(opt)) {
                return;
            }
        }
        Main.LOGGER.log(Level.FINE, String.format("EOLANG Runtime %s", Main.ver()));
        final List<String> arguments = Arrays.stream(args)
            .filter(Main::isArgument)
            .collect(Collectors.toList());
        if (arguments.isEmpty()) {
            throw new IllegalStateException(
                "The name of an object is expected as a command line argument"
            );
        }
        try {
            Main.run(arguments);
        } catch (final ExAbstract ex) {
            Main.report(opts, ex);
            System.exit(1);
        }
    }

    /**
     * Is it an argument?
     * @param arg The arg
     * @return TRUE if it's an argument
     */
    private static boolean isArgument(final String arg) {
        return !Main.isOption(arg);
    }

    /**
     * Is it an option?
     * @param arg The arg
     * @return TRUE if it's an option
     */
    private static boolean isOption(final String arg) {
        return arg.startsWith("--");
    }

    /**
     * Report exception.
     *
     * @param opts The options
     * @param thr  The cause
     */
    @SuppressWarnings("PMD.AvoidPrintStackTrace")
    private static void report(final List<String> opts, final Throwable thr) {
        if (opts.contains(Main.VERBOSE)) {
            thr.printStackTrace();
        }
        Main.print(thr);
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
     * @return True if it's time to exit
     * @throws IOException If fails
     */
    private static boolean parse(final String opt) throws IOException {
        if (Main.VERBOSE.equals(opt)) {
            Main.EOLOG.setLevel(Level.FINE);
            for (final Handler hnd : Main.EOLOG.getHandlers()) {
                hnd.setLevel(Level.FINE);
            }
        }
        boolean exit = false;
        if (Main.VERSION.equals(opt)) {
            Main.LOGGER.info(Main.ver());
            exit = true;
        }
        if (Main.HELP.equals(opt)) {
            Main.LOGGER.info(
                String.join(
                    "\n",
                    "Usage: java -cp target/classes org.eolang.Main [option...] class [argument...]",
                    "  class: Name of EO class, e.g. \"org.eolang.io.stdio\"",
                    "  argument: Value that will be wrapped as strings and passed to your EO object",
                    "  options:",
                    String.format("  %s     Print this documentation and exit", Main.HELP),
                    String.format("  %s  Print the version of this JAR and exit", Main.VERSION),
                    String.format("  %s  Print all intermediate dataization results", Main.VERBOSE)
                )
            );
            exit = true;
        }
        return exit;
    }

    /**
     * Run this opts.
     * @param opts The opts left
     */
    private static void run(final List<String> opts) {
        final String obj = opts.get(0);
        if (obj.isEmpty()) {
            throw new IllegalArgumentException(
                "The name of the object is an empty string, why?"
            );
        }
        final Phi app = Phi.Φ.take(obj);
        if (opts.size() > 1) {
            Phi args = Phi.Φ.take("org.eolang.tuple").take("empty");
            for (int idx = 1; idx < opts.size(); ++idx) {
                args = args.take("with");
                args.put(0, new Data.ToPhi(opts.get(idx)));
            }
            app.put(0, args);
        }
        final long start = System.currentTimeMillis();
        final byte[] ret = new Dataized(app).take();
        Main.LOGGER.info(
            String.format(
                "%n---%n%s%nFinished in %.02fs (%d bytes)",
                new VerboseBytesAsString(ret).get(),
                (System.currentTimeMillis() - start) / 1000.0,
                ret.length
            )
        );
    }

    /**
     * Read the version from resources and return it.
     * @return Version string
     * @throws IOException If fails
     */
    private static String ver() throws IOException {
        try (
            BufferedReader input =
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
