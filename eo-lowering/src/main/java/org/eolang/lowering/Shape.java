/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.List;

/**
 * The shape of one recorded evaluation, ready to meet the tree.
 *
 * <p>It is the identity of the site an atom fired or parked at: the
 * method that dispatched it, the key of its receiver, and the keys of
 * its arguments under their positional names. A site of the tree matches
 * when all three agree, with the argument names read either way — the
 * resolved name the record shows, or the {@code α}-positional name the
 * XMIR wrote.</p>
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
     * The keys of the arguments, in the same order.
     */
    private final List<String> keys;

    /**
     * Ctor.
     * @param verb The method that dispatched the atom
     * @param self The key of the receiver
     * @param labels The names of the arguments, in their positional order
     * @param values The keys of the arguments, in the same order
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
            good = good && this.keys.get(idx).equals(arg.value().key());
        }
        return good;
    }
}
