/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.List;
import java.util.stream.Collectors;

/**
 * The shape of one recorded evaluation, ready to meet the tree.
 *
 * <p>It is the identity of the site an atom fired or parked at: the
 * method that dispatched it, the key of its receiver, and the identities
 * of its arguments under their positional names — the key of an argument
 * that is a value, the phi text of one that is still a site, or nothing
 * at all where any argument will do. A site of the tree matches when all
 * three agree, with the argument names read either way — the resolved
 * name the record shows, or the {@code α}-positional name the XMIR
 * wrote.</p>
 *
 * @since 0.76.0
 */
public final class Shape {

    /**
     * The method that dispatched the atom.
     */
    private final String method;

    /**
     * The key of the receiver.
     */
    private final String receiver;

    /**
     * The names of the arguments, in their positional order.
     */
    private final List<String> names;

    /**
     * The identities of the arguments, in the same order.
     */
    private final List<String> keys;

    /**
     * Ctor, for the shape of exactly one site as it stands in the tree.
     * @param verb The method the site dispatches
     * @param self The key of its receiver
     * @param args The bindings of the site
     */
    public Shape(final String verb, final String self, final List<Binding> args) {
        this(
            verb,
            self,
            args.stream().map(Binding::label).collect(Collectors.toList()),
            args.stream().map(arg -> Shape.identity(arg.value())).collect(Collectors.toList())
        );
    }

    /**
     * Ctor.
     * @param verb The method that dispatched the atom
     * @param self The key of the receiver
     * @param labels The names of the arguments, in their positional order
     * @param values The identities of the arguments, in the same order,
     *  an empty one standing for any argument at all
     */
    public Shape(final String verb, final String self,
        final List<String> labels, final List<String> values) {
        this.method = verb;
        this.receiver = self;
        this.names = labels;
        this.keys = values;
    }

    /**
     * Whether a site with these parts matches this shape.
     * @param verb The method of the site
     * @param self The key of the receiver of the site
     * @param args The bindings of the site
     * @return True if the site is the one recorded
     */
    public boolean covers(final String verb, final String self, final List<Binding> args) {
        boolean good = this.method.equals(verb)
            && !self.isEmpty()
            && this.receiver.equals(self)
            && this.names.size() == args.size();
        for (int idx = 0; good && idx < args.size(); ++idx) {
            final Binding arg = args.get(idx);
            good = arg.label().equals(this.names.get(idx))
                || arg.label().equals(String.format("α%d", idx));
            final String expected = this.keys.get(idx);
            good = good
                && (expected.isEmpty() || expected.equals(Shape.identity(arg.value())));
        }
        return good;
    }

    private static String identity(final Term term) {
        String out = term.key();
        if (out.isEmpty()) {
            out = term.phi();
        }
        return out;
    }
}
