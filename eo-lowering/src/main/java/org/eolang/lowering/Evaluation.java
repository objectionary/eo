/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * One line of the evaluation protocol phino writes.
 *
 * <p>A record of three tab-separated fields is an atom that fired: the λ
 * name, the input formation, the result term. A record of two fields is
 * an atom that parked under {@code --partial}: the same, with no result.
 * The input formation is one line of phi text, and this class only
 * splits it mechanically — bindings apart at the commas of the outermost
 * formation, tracking bracket depth — without parsing what the parts
 * mean. The λ binding is omitted by phino, a binding still unset is
 * skipped, and the {@code ρ} binding is kept
 * aside: it renders the whole receiver instance, so its identity is the
 * first datum or marker inside, which is always the value binding of the
 * instance, standing before the method table.</p>
 *
 * @since 0.76.0
 */
public final class Evaluation {

    /**
     * A datum or a marker, whichever the receiver holds.
     */
    private static final Pattern IDENTITY = Pattern.compile(
        "λ ⤍ Sym_(\\w+)|Δ ⤍ (--|[0-9A-F]{2}(?:-[0-9A-F]{2})+|[0-9A-F]{2}-)"
    );

    /**
     * The line, with its fields tab-separated.
     */
    private final String line;

    /**
     * Ctor.
     * @param text One line of the protocol file
     */
    public Evaluation(final String text) {
        this.line = text;
    }

    /**
     * The λ name of the atom.
     * @return The name, such as {@code L_number_plus} or {@code Sym_v0}
     */
    public String name() {
        return this.fields()[0];
    }

    /**
     * Whether the atom parked instead of firing.
     * @return True if the record has no result term
     */
    public boolean parked() {
        return this.fields().length < 3;
    }

    /**
     * The result term of the fired atom.
     * @return The term, such as a {@code Φ.number} application
     */
    public String result() {
        if (this.parked()) {
            throw new IllegalStateException(
                String.format("The atom '%s' parked, so it has no result", this.name())
            );
        }
        return this.fields()[2];
    }

    /**
     * The named bindings of the input formation, in their written order.
     * @return Binding names mapped to their terms, without {@code ρ} and unset ones
     */
    public Map<String, String> bindings() {
        final Map<String, String> out = new LinkedHashMap<>();
        for (final String part : this.parts()) {
            final int arrow = part.indexOf(" ↦ ");
            if (arrow < 0) {
                continue;
            }
            final String name = part.substring(0, arrow);
            final String term = part.substring(arrow + 3);
            if (!"ρ".equals(name) && !"∅".equals(term)) {
                out.put(name, term);
            }
        }
        return out;
    }

    /**
     * The identity of the receiver.
     * @return Either {@code sym:<name>} or {@code Δ:<hex>}
     */
    public String receiver() {
        String rho = "";
        for (final String part : this.parts()) {
            if (part.startsWith("ρ ↦ ")) {
                rho = part;
                break;
            }
        }
        final Matcher found = Evaluation.IDENTITY.matcher(rho);
        if (!found.find()) {
            throw new IllegalStateException(
                String.format(
                    "The receiver of the atom '%s' shows neither a datum nor a marker",
                    this.name()
                )
            );
        }
        final String out;
        if (found.group(1) == null) {
            out = String.format("Δ:%s", found.group(2));
        } else {
            out = String.format("sym:%s", found.group(1));
        }
        return out;
    }

    private String[] fields() {
        return this.line.split("\t");
    }

    private Iterable<String> parts() {
        final String input = this.fields()[1].trim();
        final List<String> out = new ArrayList<>(4);
        final String inner = input.substring(1, input.length() - 1).trim();
        int depth = 0;
        int start = 0;
        for (int pos = 0; pos < inner.length(); ++pos) {
            final char sym = inner.charAt(pos);
            if (sym == '⟦' || sym == '(') {
                ++depth;
            } else if (sym == '⟧' || sym == ')') {
                --depth;
            } else if (sym == ',' && depth == 0) {
                out.add(inner.substring(start, pos).trim());
                start = pos + 1;
            }
        }
        if (start < inner.length()) {
            out.add(inner.substring(start).trim());
        }
        return out;
    }
}
