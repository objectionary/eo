/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import org.eolang.lowering.Box;
import org.eolang.lowering.Carrier;
import org.eolang.lowering.Symbols;

/**
 * The operands of one fire, each asked of phino.
 *
 * <p>The engine reads no φ. Whatever it needs to know about an operand,
 * it asks phino for by the id of the fire and the name of the attribute,
 * and reads off the facts phino answers with: the λ a symbol is stuck on,
 * the Δ a datum carries, the global object a literal applies. An operand
 * whose forma the operation knows costs one question, reduced, and a
 * datum takes that forma, since bytes carry no type of their own, while a
 * symbol of no carrier takes it too, with the operation as the witness.
 * An operand of no known forma, such as an arm of an if, a void of a box
 * nobody typed or an operand of an operation on bytes, which any datum
 * is, is first asked for as written, since a symbol bound as it is must
 * not be dataized and a literal names its forma only in the global object
 * it is dispatched by, and is reduced after that. A bool is asked for as
 * written too, since a bool marker reduces to a fork over its own truth,
 * while the symbol it stands for sits under its {@code if}, where the
 * engine put it, and is read back from there by its path. Only an
 * operation on bytes takes the fork, since it is after the bytes.</p>
 *
 * @since 0.76.0
 */
final class Operands {

    /**
     * The name of a symbol marker.
     */
    private static final Pattern SYMBOL = Pattern.compile("S\\d+");

    /**
     * The formas under which an operand is first asked for as written,
     * since they say nothing about it or a marker may stand in it.
     */
    private static final List<String> PROBED = Arrays.asList("", "object", "bytes", "bool");

    /**
     * The global objects a literal is dispatched by, each the forma of it.
     */
    private static final List<String> LITERALS = Arrays.asList("number", "string", "bytes");

    /**
     * The id of the fire.
     */
    private final int fire;

    /**
     * The wire.
     */
    private final Channel channel;

    /**
     * The table.
     */
    private final Symbols table;

    /**
     * Ctor.
     *
     * @param id The id of the fire
     * @param wire The wire
     * @param symbols The table
     */
    Operands(final int id, final Channel wire, final Symbols symbols) {
        this.fire = id;
        this.channel = wire;
        this.table = symbols;
    }

    /**
     * Whether the name is bound at all.
     *
     * @param name The name of the attribute
     * @return True if the fire binds it to something other than a void
     * @throws IOException If the wire fails
     * @throws InterruptedException If the wait is interrupted
     */
    boolean bound(final String name) throws IOException, InterruptedException {
        return !this.channel.ask(this.fire, name, false).vacant();
    }

    /**
     * The key of a reduced operand.
     *
     * @param name The name of the attribute
     * @param forma The forma the operation expects, or an empty string
     * @return The key, {@code sym:S4} or {@code number:HEX}
     * @throws IOException If the wire fails
     * @throws InterruptedException If the wait is interrupted
     */
    String of(final String name, final String forma)
        throws IOException, InterruptedException {
        String key = "";
        String kind = forma;
        if ("object".equals(forma)) {
            kind = "";
        }
        if (Operands.PROBED.contains(forma)) {
            final Answer written = this.channel.ask(this.fire, name, false);
            key = Operands.key(written, kind);
            if (Operands.LITERALS.contains(written.head())) {
                kind = written.head();
            } else if ("bool".equals(written.head()) && !"bytes".equals(forma)) {
                key = this.guarded(name);
            }
        }
        if (key.isEmpty()) {
            key = this.keyed(name, kind);
        }
        return key;
    }

    /**
     * The key of the receiver of a box.
     *
     * @param box The box asking
     * @return The key, or an empty string when the receiver is the lexical parent
     * @throws IOException If the wire fails
     * @throws InterruptedException If the wait is interrupted
     */
    String receiver(final Box box) throws IOException, InterruptedException {
        final String key;
        if ("tuple".equals(box.parent())) {
            key = this.tuple();
        } else if (new Carrier(String.format("Φ.%s", box.parent())).data()) {
            key = this.keyed("ρ", box.parent());
        } else {
            key = this.parent(box);
        }
        return key;
    }

    private String tuple() throws IOException, InterruptedException {
        final Answer length = this.channel.ask(this.fire, "ρ.length", true);
        if (!Operands.SYMBOL.matcher(length.lambda()).matches()) {
            throw new IllegalStateException(
                String.format(
                    "The receiver of fire #%d is a tuple of no symbolic length: %s",
                    this.fire, length.node()
                )
            );
        }
        return this.table.row(length.lambda()).get(3);
    }

    private String parent(final Box box) throws IOException, InterruptedException {
        final Answer written = this.channel.ask(this.fire, "ρ", false);
        final String key = Operands.key(written, "");
        if (key.isEmpty() && !box.lambda().equals(
            this.channel.ask(
                this.fire, String.format("ρ.%s", box.name()), false
            ).lambda()
        )) {
            throw new IllegalStateException(
                String.format(
                    "The receiver of fire #%d is neither a value nor the parent of '%s': %s",
                    this.fire, box.lambda(), written.node()
                )
            );
        }
        return key;
    }

    private String guarded(final String name) throws IOException, InterruptedException {
        final Answer guard = this.channel.ask(
            this.fire, String.format("%s.if.guard", name), false
        );
        if (!Operands.SYMBOL.matcher(guard.lambda()).matches()) {
            throw new IllegalStateException(
                String.format(
                    "The bool '%s' of fire #%d is a marker of no symbol: %s",
                    name, this.fire, guard.node()
                )
            );
        }
        return String.format("sym:%s", guard.lambda());
    }

    private String keyed(final String name, final String forma)
        throws IOException, InterruptedException {
        final Answer reduced = this.channel.ask(this.fire, name, true);
        final String key = Operands.key(reduced, forma);
        if (key.isEmpty()) {
            throw new IllegalStateException(
                String.format(
                    "The operand '%s' of fire #%d is neither a symbol nor data: %s",
                    name, this.fire, reduced.node()
                )
            );
        }
        if (key.startsWith("sym:")) {
            this.table.witnessed(reduced.lambda(), forma);
        }
        return key;
    }

    private static String key(final Answer answer, final String forma) {
        final String key;
        if (Operands.SYMBOL.matcher(answer.lambda()).matches()) {
            key = String.format("sym:%s", answer.lambda());
        } else if (answer.data().isEmpty()) {
            key = "";
        } else if (forma.isEmpty()) {
            key = String.format("bytes:%s", answer.data());
        } else {
            key = String.format("%s:%s", forma, answer.data());
        }
        return key;
    }
}
