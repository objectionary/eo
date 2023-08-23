/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
 * Class to manipulate eo objects within "Universe" paradigm.
 * @since 0.30
 */
public class Universe {

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
    public Universe(final Phi connector, final Map<Integer, Phi> indexed) {
        this.connector = connector;
        this.indexed = indexed;
    }

    /**
     * Ctor.
     * @param connector Connector.
     */
    public Universe(final Phi connector) {
        this(
            connector, new HashMap<>()
        );
    }

    /**
     * Ctor.
     */
    public Universe() {
        this(Phi.Φ);
    }

    /**
     * Finds vertex of eo object by its location.
     * @param name Relative location of the object to find like "^.^.some-obj".
     * @return Vertex of the object to find.
     */
    public int find(final String name) {
        Phi accum;
        final String[] atts = Universe.replace(name)
            .split("\\.");
        if (atts[0].equals("")) {
            accum = Phi.Φ;
            atts[0] = "";
        } else if (atts[0].equals("ρ")) {
            accum = this.connector;
        } else if (atts[0].equals("$")) {
            accum = this.connector;
            atts[0] = "";
        } else {
            throw new ExFailure(
                String.format(
                    "Universe.find starts with %s, but it should start with Q or ^ or $ only",
                    atts[0]
                )
            );
        }
        for (final String att: atts) {
            if (!"".equals(att)) {
                accum = accum.attr(att).get();
            }
        }
        this.indexed.putIfAbsent(accum.hashCode(), accum);
        return accum.hashCode();
    }

    /**
     * Puts data to eo object by vertex.
     * @param vertex Vertex off object.
     * @param bytes Data to put.
     * @checkstyle NonStaticMethodCheck (4 lines)
     */
    public void put(final int vertex, final byte[] bytes) {
        //Empty yet.
    }

    /**
     * Binds child to parent.
     * @param parent Vertex of the parent eo object.
     * @param child Vertex of the child eo object.
     * @param att Name of attribute.
     * @todo #2237:45min Implement the "bind" method. It has tp
     *  put data to eo object by vertex. It does nothing now
     *  but it is called from rust via jni call_method function.
     * @checkstyle NonStaticMethodCheck (4 lines)
     */
    public void bind(final int parent, final int child, final String att) {
        //Empty yet.
    }

    /**
     * Copies the eo object.
     * @param vertex Vertex of object to copy.
     * @return Vertex of the copy.
     * @todo #2237:45min Implement the "copy" method. Now it does
     *  nothing and created to check rust2java interaction. This
     *  method relates to building a new eo object in rust insert.
     * @checkstyle NonStaticMethodCheck (4 lines)
     */
    public int copy(final int vertex) {
        return vertex;
    }

    /**
     * Dataizes the eo object by vertex and return byte array.
     * @param vertex Vertex of eo-object.
     * @return Raw data.
     */
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
