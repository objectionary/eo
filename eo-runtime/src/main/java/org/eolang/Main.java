/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

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
     * Handler for EO runtime logs.
     */
    private static final Handler HANDLER = Main.handler();

    /**
     * Lock guarding the EO runtime logger configuration.
     */
    private static final ReentrantLock LOCK = new ReentrantLock();

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
        final List<String> opts = new ArrayList<>(0);
        final List<String> arguments = new ArrayList<>(0);
        for (final String arg : args) {
            if (arguments.isEmpty() && Main.isOption(arg)) {
                opts.add(arg);
            } else {
                arguments.add(arg);
            }
        }
        for (final String opt : opts) {
            if (Main.parse(opt)) {
                return;
            }
        }
        Main.LOGGER.log(Level.FINE, String.format("EOLANG Runtime %s", Main.ver()));
        if (arguments.isEmpty()) {
            throw new ExFailure(
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

    private static boolean isOption(final String arg) {
        return arg.startsWith("--");
    }

    @SuppressWarnings("PMD.AvoidPrintStackTrace")
    private static void report(final List<String> opts, final Throwable thr) {
        if (opts.contains(Main.VERBOSE)) {
            thr.printStackTrace();
        }
        Main.print(thr);
    }

    private static void print(final Throwable thr) {
        Main.LOGGER.log(Level.SEVERE, thr.getMessage());
        final Throwable cause = thr.getCause();
        if (cause != null) {
            Main.print(cause);
        }
    }

    private static void setup() {
        Main.LOCK.lock();
        try {
            if (Arrays.stream(Main.EOLOG.getHandlers()).noneMatch(
                handler -> Objects.equals(handler, Main.HANDLER)
            )) {
                Main.EOLOG.addHandler(Main.HANDLER);
            }
        } finally {
            Main.LOCK.unlock();
        }
        Main.EOLOG.setUseParentHandlers(false);
    }

    private static Handler handler() {
        final Handler handler = new ConsoleHandler();
        handler.setFormatter(
            new Formatter() {
                @Override
                public String format(final LogRecord rec) {
                    return String.format("%s%n", rec.getMessage());
                }
            }
        );
        return handler;
    }

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
                    System.lineSeparator(),
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

    private static void run(final List<String> opts) {
        final String obj = opts.get(0);
        if (obj.isEmpty()) {
            throw new ExFailure(
                "The name of the object is an empty string, why?"
            );
        }
        final Phi app = Phi.Φ.take(obj);
        if (opts.size() > 1) {
            Phi args = Phi.Φ.take("tuple").take("empty");
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
