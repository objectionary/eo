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

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * Default implementation that can be used on the java side.
 * @since 0.32
 */
@Versionized
public final class UniverseDefault implements Universe {

    /**
     * Connector to eo objects.
     */
    private final Phi connector;

    /**
     * EO objects that were already found.
     */
    private final Map<Integer, Phi> indexed;

    /**
     * Ctor.
     * @param connector Connector.
     * @param indexed Map to index eo objects.
     */
    public UniverseDefault(final Phi connector, final Map<Integer, Phi> indexed) {
        this.connector = connector;
        this.indexed = indexed;
    }

    /**
     * Ctor.
     * @param connector Connector.
     */
    public UniverseDefault(final Phi connector) {
        this(
            connector, new HashMap<>()
        );
    }

    /**
     * Ctor.
     */
    public UniverseDefault() {
        this(Phi.Φ);
    }

    @Override
    public int find(final String name) {
        if (name == null) {
            throw new IllegalArgumentException(
                "Argument name is null"
            );
        }
        Phi accum;
        final String[] atts = UniverseDefault.replace(name)
            .split("\\.");
        if ("Q".equals(atts[0])) {
            accum = Phi.Φ;
        } else if ("$".equals(atts[0])) {
            accum = this.connector;
        } else {
            throw new ExFailure(
                String.format(
                    "Universe.find starts with %s, but it should start with Q or $ only",
                    atts[0]
                )
            );
        }
        atts[0] = "";
        for (final String att: atts) {
            if (!"".equals(att)) {
                accum = accum.attr(att).get();
            }
        }
        this.indexed.putIfAbsent(accum.hashCode(), accum);
        return accum.hashCode();
    }

    @Override
    public void put(final int vertex, final byte[] bytes) {
        this.get(vertex).attr("Δ").put(
            new Data.Value<>(bytes)
        );
    }

    @Override
    public void bind(final int parent, final int child, final String att) {
        this.get(parent)
            .attr(att)
            .put(this.get(child));
    }

    @Override
    public int copy(final int vertex) {
        final Phi copy = this.get(vertex).copy();
        this.indexed.putIfAbsent(copy.hashCode(), copy);
        return copy.hashCode();
    }

    @Override
    public byte[] dataize(final int vertex) {
        return new Param(
            this.get(vertex),
            "Δ"
        ).asBytes().take();
    }

    /**
     * Find phi by vertex.
     * @param vertex Vertex.
     * @return Phi.
     * @throws ExFailure if vertex does not exist in the map.
     */
    private Phi get(final int vertex) {
        return Optional.ofNullable(
            this.indexed.get(vertex)
        ).orElseThrow(
            () -> new ExFailure(
                String.format(
                    "Phi object with vertex %d was not indexed.",
                    vertex
                )
            )
        );
    }

    /**
     * Replaces specific eo symbols to java symbols.
     * @param name Name of eo object.
     * @return Correct location.
     */
    private static String replace(final String name) {
        final StringBuilder builder = new StringBuilder(name.length());
        for (int iter = 0; iter < name.length(); iter += 1) {
            final char cur = name.charAt(iter);
            switch (cur) {
                case '^':
                    builder.append('ρ');
                    break;
                case '@':
                    builder.append('φ');
                    break;
                case '&':
                    builder.append('σ');
                    break;
                default:
                    builder.append(cur);
                    break;
            }
        }
        return builder.toString();
    }

}
