/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import org.cactoos.Text;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * The symbol table of one run, kept in a tab-separated file.
 *
 * <p>It takes the path of the file. Given a row — an argument, an
 * operation on operands, a fork, an entry into another fragment — it mints
 * a symbol and answers it, and it answers the carrier and the cells of any
 * symbol it holds. The build seeds the table with the arguments, the
 * engine appends the rest from its own process, and the build reads it all
 * back as the program of the fragment. A row asked for twice answers the
 * symbol it has already, which is how repeated work is computed once.</p>
 *
 * @since 0.76.0
 */
public final class Symbols {

    /**
     * The formas a value of the engine carries as data.
     */
    private static final List<String> DATA = Arrays.asList("number", "bool", "bytes", "string");

    /**
     * The file.
     */
    private final Path file;

    /**
     * The lock over the file.
     */
    private final Lock lock;

    /**
     * Ctor.
     *
     * @param table The file
     */
    public Symbols(final Path table) {
        this(table, new ReentrantLock());
    }

    /**
     * Ctor.
     *
     * @param table The file
     * @param mutex The lock over the file
     */
    Symbols(final Path table, final Lock mutex) {
        this.file = table;
        this.lock = mutex;
    }

    /**
     * Mint a symbol for a row, or find the one it has.
     *
     * @param carrier The carrier of the value
     * @param cells The kind and the operands
     * @return The symbol
     * @throws IOException If the file cannot be read or written
     */
    public String minted(final String carrier, final List<String> cells) throws IOException {
        this.lock.lock();
        try {
            final List<String> tail = new ArrayList<>(cells.size() + 1);
            tail.add(carrier);
            tail.addAll(cells);
            String out = "";
            for (final List<String> row : this.rows()) {
                if (row.subList(1, row.size()).equals(tail)) {
                    out = row.get(0);
                    break;
                }
            }
            if (out.isEmpty()) {
                out = this.fresh(carrier, cells);
            }
            return out;
        } finally {
            this.lock.unlock();
        }
    }

    /**
     * Mint a fresh symbol for a row, whether or not the table holds it already.
     *
     * @param carrier The carrier of the value
     * @param cells The kind and the operands
     * @return The symbol
     * @throws IOException If the file cannot be read or written
     */
    public String fresh(final String carrier, final List<String> cells) throws IOException {
        this.lock.lock();
        try {
            final Set<String> seen = new LinkedHashSet<>(0);
            for (final List<String> row : this.rows()) {
                seen.add(row.get(0));
            }
            final String sym = String.format("S%d", seen.size() + 1);
            final List<String> tail = new ArrayList<>(cells.size() + 1);
            tail.add(carrier);
            tail.addAll(cells);
            this.record(sym, tail.toArray(new String[0]));
            return sym;
        } finally {
            this.lock.unlock();
        }
    }

    /**
     * Append a row.
     *
     * @param sym The symbol
     * @param cells The rest of the row
     * @throws IOException If the file cannot be written
     */
    public void record(final String sym, final String... cells) throws IOException {
        this.lock.lock();
        try {
            final List<String> row = new ArrayList<>(cells.length + 1);
            row.add(sym);
            row.addAll(Arrays.asList(cells));
            Files.write(
                this.file,
                String.join("\t", row)
                    .concat(System.lineSeparator())
                    .getBytes(StandardCharsets.UTF_8),
                StandardOpenOption.CREATE, StandardOpenOption.APPEND
            );
        } finally {
            this.lock.unlock();
        }
    }

    /**
     * Change the carrier of a symbol.
     *
     * @param sym The symbol
     * @param carrier The carrier
     * @throws IOException If the file cannot be read or written
     */
    public void retyped(final String sym, final String carrier) throws IOException {
        this.lock.lock();
        try {
            final List<String> lines = new ArrayList<>(0);
            boolean pending = true;
            for (final List<String> row : this.rows()) {
                final List<String> cells = new ArrayList<>(row);
                if (pending && cells.get(0).equals(sym) && cells.size() > 2) {
                    cells.set(1, carrier);
                    pending = false;
                }
                lines.add(String.join("\t", cells));
            }
            Files.write(
                this.file,
                String.join(System.lineSeparator(), lines)
                    .concat(System.lineSeparator())
                    .getBytes(StandardCharsets.UTF_8)
            );
        } finally {
            this.lock.unlock();
        }
    }

    /**
     * The carrier of a symbol.
     *
     * @param sym The symbol
     * @return The carrier
     */
    public String carrier(final String sym) {
        return this.row(sym).get(1);
    }

    /**
     * Retype a symbol of no carrier to the data forma a witness saw it
     * carry, such as the operation that consumes it or the fork whose
     * other arm carries it, and leave every other symbol as it is.
     *
     * @param sym The symbol
     * @param forma The forma witnessed
     * @throws IOException If the file cannot be read or written
     */
    public void witnessed(final String sym, final String forma) throws IOException {
        if (Symbols.DATA.contains(forma)
            && this.rows().stream().anyMatch(
                row -> row.get(0).equals(sym) && "object".equals(row.get(1))
            )) {
            this.retyped(sym, forma);
        }
    }

    /**
     * The row of a symbol, the one with a carrier and a kind.
     *
     * @param sym The symbol
     * @return The cells of the row
     */
    public List<String> row(final String sym) {
        return this.rows().stream()
            .filter(row -> row.get(0).equals(sym) && row.size() > 2)
            .findFirst().orElseThrow(
                () -> new IllegalStateException(
                    String.format("The table has no row for the symbol '%s'", sym)
                )
            );
    }

    /**
     * All rows.
     *
     * @return The rows, each as its cells
     */
    List<List<String>> rows() {
        this.lock.lock();
        try {
            final List<List<String>> out = new ArrayList<>(0);
            if (Files.exists(this.file)) {
                for (final Text line : this.lines()) {
                    final String row = new UncheckedText(line).asString();
                    if (!row.isEmpty()) {
                        out.add(Arrays.asList(row.split("\t", -1)));
                    }
                }
            }
            return out;
        } finally {
            this.lock.unlock();
        }
    }

    private Iterable<Text> lines() {
        return new Split(new TextOf(this.file), "\\R");
    }
}
