/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * The table of symbols read as programs: the rows a symbol reaches are the
 * protocol of the atom that computes it, and the {@code void} rows among
 * them are its inputs.
 *
 * <p>A frame of the program, the top or an arm of a fork, computes what
 * its own answer needs and nothing else, in the order of the dependencies,
 * skipping what an enclosing frame computed already. A symbol two arms
 * share is computed in both, since only one of them runs, and one an arm
 * shares with the code after the fork is computed twice, which is what the
 * program did before it was lowered, since the arm was reduced only when
 * its guard held.</p>
 *
 * @since 0.77.0
 */
final class Table {

    /**
     * The table.
     */
    private final Symbols symbols;

    /**
     * Ctor.
     *
     * @param table The table
     */
    Table(final Symbols table) {
        this.symbols = table;
    }

    /**
     * The void a symbol reads, or an attribute of one, as a path from the
     * fragment, such as {@code a}, {@code ρ.a} or {@code t.length}.
     *
     * @param sym The symbol
     * @return The path, or the empty string when the symbol computes
     */
    String reference(final String sym) {
        final List<String> row = this.symbols.row(sym);
        String out = "";
        if ("void".equals(row.get(2))) {
            out = row.get(3);
        } else if ("attr".equals(row.get(2))) {
            final String head = this.reference(row.get(3).substring(4));
            if (!head.isEmpty()) {
                out = String.format("%s.%s", head, row.get(4));
            }
        }
        return out;
    }

    /**
     * The symbol a part of a tuple is read off.
     *
     * @param sym The symbol
     * @return The symbol of the tuple, or the empty string when the
     *  symbol is no {@code attr} row
     */
    String receiver(final String sym) {
        final List<String> row = this.symbols.row(sym);
        String out = "";
        if ("attr".equals(row.get(2))) {
            out = row.get(3).substring(4);
        }
        return out;
    }

    /**
     * The inputs of the program of a symbol, in the order they are first
     * read: the path of a void to its forma, or {@code box:<locator>} to
     * {@code formation} for a formation entered where it stands.
     *
     * @param sym The symbol
     * @return The inputs
     */
    Map<String, String> inputs(final String sym) {
        final Map<String, String> out = new LinkedHashMap<>(0);
        this.gathered(String.format("sym:%s", sym), out, new HashSet<>(0));
        return out;
    }

    /**
     * The program of a symbol.
     *
     * @param sym The symbol
     * @param names The Java name of each input, by its key in {@link #inputs}
     * @return The program
     */
    Program program(final String sym, final Map<String, String> names) {
        final Map<String, String> inputs = this.inputs(sym);
        final Map<String, String> renamed = new LinkedHashMap<>(inputs.size());
        for (final Map.Entry<String, String> input : inputs.entrySet()) {
            if (!names.containsKey(input.getKey())) {
                throw new IllegalStateException(
                    String.format("The input '%s' of '%s' has no name", input.getKey(), sym)
                );
            }
            renamed.put(names.get(input.getKey()), input.getValue());
        }
        final List<String> voids = new ArrayList<>(inputs.keySet());
        return new Program(
            Collections.singletonList(
                new Body(
                    "", 0, new ArrayList<>(inputs.values()),
                    this.protocol(String.format("sym:%s", sym), new HashSet<>(0), voids)
                )
            ),
            renamed
        );
    }

    private void gathered(final String key, final Map<String, String> out,
        final Set<String> seen) {
        if (key.startsWith("sym:") && seen.add(key)) {
            final List<String> row = this.symbols.row(key.substring(4));
            final String kind = row.get(2);
            if ("void".equals(kind)) {
                out.put(row.get(3), row.get(1));
            } else if ("fork".equals(kind)) {
                this.gathered(row.get(3), out, seen);
                for (final String arm : Arrays.asList("left", "right")) {
                    this.gathered(this.answered(row.get(0), arm), out, seen);
                }
            } else if ("box".equals(kind)) {
                if (row.stream().noneMatch(cell -> cell.startsWith("ρ="))) {
                    out.put(String.format("box:%s", row.get(3)), "formation");
                }
                for (final String cell : row.subList(4, row.size())) {
                    this.gathered(cell.substring(cell.indexOf('=') + 1), out, seen);
                }
            } else {
                for (final String cell : row.subList(3, row.size())) {
                    this.gathered(cell, out, seen);
                }
            }
        }
    }

    private Protocol protocol(final String answer, final Set<String> defined,
        final List<String> voids) {
        final List<Step> moves = new ArrayList<>(0);
        this.stepped(answer, defined, voids, moves);
        return new Protocol(moves, this.key(answer, voids), this.carried(answer));
    }

    private void stepped(final String key, final Set<String> defined,
        final List<String> voids, final List<Step> moves) {
        if (key.startsWith("sym:")) {
            final String sym = key.substring(4);
            final List<String> row = this.symbols.row(sym);
            final String kind = row.get(2);
            if (!"void".equals(kind) && defined.add(sym)) {
                final String label = sym.toLowerCase(Locale.ENGLISH);
                if ("attr".equals(kind)) {
                    this.stepped(row.get(3), defined, voids, moves);
                    moves.add(
                        new Dispatch(
                            label, row.get(4),
                            Collections.singletonList(this.key(row.get(3), voids)), row.get(1)
                        )
                    );
                } else if ("fork".equals(kind)) {
                    this.stepped(row.get(3), defined, voids, moves);
                    moves.add(
                        new Fork(
                            label, "L_fork", this.key(row.get(3), voids),
                            this.protocol(this.answered(sym, "left"), new HashSet<>(defined), voids),
                            this.protocol(this.answered(sym, "right"), new HashSet<>(defined), voids)
                        )
                    );
                } else if ("box".equals(kind)) {
                    moves.add(this.entered(label, row, defined, voids, moves));
                } else {
                    final List<String> keys = new ArrayList<>(row.size() - 3);
                    for (final String cell : row.subList(3, row.size())) {
                        this.stepped(cell, defined, voids, moves);
                        keys.add(this.key(cell, voids));
                    }
                    moves.add(new Application(label, kind, keys));
                }
            }
        }
    }

    private Step entered(final String label, final List<String> row,
        final Set<String> defined, final List<String> voids, final List<Step> moves) {
        final List<String> keys = new ArrayList<>(row.size() - 3);
        final List<String> names = new ArrayList<>(row.size() - 4);
        keys.add(
            String.format("sym:v%d", voids.indexOf(String.format("box:%s", row.get(3))))
        );
        for (final String cell : row.subList(4, row.size())) {
            final String value = cell.substring(cell.indexOf('=') + 1);
            this.stepped(value, defined, voids, moves);
            if (cell.startsWith("ρ=")) {
                keys.set(0, this.key(value, voids));
            } else {
                names.add(cell.substring(0, cell.indexOf('=')));
                keys.add(this.key(value, voids));
            }
        }
        return new Entry(
            label, String.format("%s(%s)", row.get(3), String.join(",", names)), keys, row.get(1)
        );
    }

    private String answered(final String sym, final String arm) {
        return this.symbols.rows().stream()
            .filter(
                row -> row.size() == 4 && row.get(0).equals(sym)
                    && row.get(1).equals(arm) && "answer".equals(row.get(2))
            )
            .map(row -> row.get(3))
            .findFirst()
            .orElseThrow(
                () -> new IllegalStateException(
                    String.format("The fork '%s' has no answer in its %s arm", sym, arm)
                )
            );
    }

    private String key(final String raw, final List<String> voids) {
        final String out;
        if (raw.startsWith("sym:")) {
            final List<String> row = this.symbols.row(raw.substring(4));
            if ("void".equals(row.get(2))) {
                out = String.format("sym:v%d", voids.indexOf(row.get(3)));
            } else {
                out = String.format("sym:%s", raw.substring(4).toLowerCase(Locale.ENGLISH));
            }
        } else {
            out = raw;
        }
        return out;
    }

    private String carried(final String raw) {
        final String out;
        if (raw.startsWith("sym:")) {
            out = this.symbols.carrier(raw.substring(4));
        } else {
            out = raw.substring(0, raw.indexOf(':'));
        }
        return out;
    }
}
