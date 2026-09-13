/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * The symbol table of one run, a tab-separated file.
 *
 * <p>Every row starts with a symbol and, in most rows, the carrier of its
 * value and the kind of the row: {@code S1 number void a} is an input,
 * {@code S4 number L_number_plus sym:S1 sym:S3} an operation on two
 * operands, {@code S5 number fork sym:S4} a fork on a bool, and
 * {@code S9 bytes box Φ.demo.f x=sym:S1} an entry into another fragment.
 * The rows {@code S5 left}, {@code S5 left answer sym:S3} and
 * {@code S5 end} carry no carrier: they open and close the arms of a
 * fork. The build side seeds the table with the inputs, the engine
 * appends the rest, and the build side reads it all back as the program
 * of the fragment.</p>
 *
 * <p>A row minted for the second time answers the symbol it has already,
 * which is how common subexpressions are eliminated. A fork is never the
 * same row twice, since its arms are not in the row.</p>
 *
 * @since 0.76.0
 */
public final class Symbols {

    /**
     * The file.
     */
    private final Path file;

    /**
     * Ctor.
     *
     * @param table The file
     */
    public Symbols(final Path table) {
        this.file = table;
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
        synchronized (this.file) {
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
        synchronized (this.file) {
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
        synchronized (this.file) {
            final List<String> row = new ArrayList<>(cells.length + 1);
            row.add(sym);
            row.addAll(Arrays.asList(cells));
            Files.write(
                this.file,
                String.join("\t", row).concat("\n").getBytes(StandardCharsets.UTF_8),
                StandardOpenOption.CREATE, StandardOpenOption.APPEND
            );
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
        synchronized (this.file) {
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
                String.join("\n", lines).concat("\n").getBytes(StandardCharsets.UTF_8)
            );
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
     * The row of a symbol, the one with a carrier and a kind.
     *
     * @param sym The symbol
     * @return The cells of the row
     */
    public List<String> row(final String sym) {
        return this.rows().stream()
            .filter(row -> row.get(0).equals(sym) && row.size() > 2)
            .findFirst()
            .orElseThrow(
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
    public List<List<String>> rows() {
        synchronized (this.file) {
            try {
                final List<List<String>> out;
                if (Files.exists(this.file)) {
                    out = Files.readAllLines(this.file, StandardCharsets.UTF_8).stream()
                        .filter(line -> !line.isEmpty())
                        .map(line -> Arrays.asList(line.split("\t", -1)))
                        .collect(Collectors.toList());
                } else {
                    out = new ArrayList<>(0);
                }
                return out;
            } catch (final IOException ex) {
                throw new UncheckedIOException(ex);
            }
        }
    }
}
